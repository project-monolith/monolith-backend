{-
    Copyright (C) 2015  Michael Dunsmuir

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- | This service implements the `Monolith.Backend.RealtimeData` interface.
-- It fetches data from the One Bus Away REST API.
module Monolith.Backend.Services.RealtimeData.ObaRest 
  ( ObaRestConfig(..)
  , newHandle
  , module RD
  ) where

import Control.Applicative
import Control.Monad (when, guard, mzero)
import Control.Exception
import Network.HTTP
import Data.Maybe
import Data.List (intersperse)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import Data.ByteString.Lazy (pack)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Monolith.Backend.Services.RealtimeData as RD
import qualified Monolith.Backend.Services.RealtimeData.Types as RDT

data ObaRestConfig = ObaRestConfig
  { obaRestApiKey :: String
  , obaRestRootUrl :: String
  } deriving Show

$(deriveJSON defaultOptions ''ObaRestConfig)

-- * Wrapper types for parsing the realtime data types without overlapping
-- instances

newtype ObaTrip = ObaTrip 
  { obaGetTrip :: RDT.Trip
  } deriving Show

instance FromJSON ObaTrip where
  parseJSON (Object o) = do
    tripId <- o .: "tripId"
    routeId <- o .: "routeId"
    predictedB <- o .: "predicted"
    let predicted = if predictedB then RDT.Realtime else RDT.Scheduled
    arrival <- 
      (`div` 1000) <$> o .: (if predictedB then "predictedArrivalTime" else "scheduledArrivalTime")
    headsign <- o .: "tripHeadsign"
    return $ ObaTrip $ RDT.Trip arrival tripId routeId predicted headsign
  parseJSON _ = mzero

newtype ObaRoute = ObaRoute
  { obaGetRoute :: RDT.Route
  } deriving Show

instance FromJSON ObaRoute where
  parseJSON (Object o) =
    let route = RDT.Route <$> o .: "id" <*> o .: "shortName" <*> 
                  o .: "description" <*> return S.empty
    in  ObaRoute <$> route

newtype ObaStop = ObaStop RDT.Stop

instance FromJSON ObaStop where
  parseJSON v@(Object o) = do
    -- get the status code and the timestamp; verify that the status code is 200
    code <- o .: "code" :: Parser Int
    guard $ code == 200
    timestamp <- (`div` 1000) <$> o .: "currentTime" :: Parser Int

    -- extract the sub-objects of interest; this is really ugly and there must be
    -- a better way?

{-  
    baseData <- getAttr "data" o
    tripsValue <- getAttr "arrivalsAndDepartures" =<< getAttr "entry" baseData 
    routesValue <- getAttr "routes" =<< getAttr "references" baseData
-}

    let maybeData = jsonObjectLookup "data" v

        maybeTripsValue = do
          datas <- maybeData
          entry <- jsonObjectLookup "entry" datas
          jsonObjectLookup "arrivalsAndDepartures" entry

        maybeRoutesValue = do
          datas <- maybeData
          refs <- jsonObjectLookup "references" datas
          jsonObjectLookup "routes" refs

    guard $ all isJust [maybeTripsValue, maybeRoutesValue]
    let Just tripsValue = maybeTripsValue
        Just routesValue = maybeRoutesValue

    -- parse the trips
    trips <- fmap obaGetTrip <$> parseJSON tripsValue :: Parser [RDT.Trip]
    routes <- fmap obaGetRoute <$> parseJSON routesValue :: Parser [RDT.Route]
  
    let routeMap =
          foldr (\r@(RDT.Route routeId _ _ _) m -> HM.insert routeId r m) HM.empty routes
        f t@(RDT.Trip _ _ routeId _ _) m =
          let g (RDT.Route id num desc trips) = RDT.Route id num desc (S.insert t trips)
          in  HM.adjust g routeId m
        routesWithTrips = foldr f routeMap trips
        routesWithNotNullTrips =
          filter (not . S.null . RDT.routeTrips) $ HM.elems routesWithTrips

    return $ ObaStop $ RDT.Stop "foo" "bar" routesWithNotNullTrips timestamp

getAttr :: T.Text -> Object -> Parser Object
getAttr key object = do
  value <- object .: key :: Parser Value
  getObject value

getObject :: Value -> Parser Object
getObject (Object o) = return o
getObject _ = empty

jsonObjectLookup :: T.Text -> Value -> Maybe Value
jsonObjectLookup key (Object o) = HM.lookup key o
jsonObjectLookup key _ = Nothing
    
-- * Get a new handle

newHandle :: ObaRestConfig -> RD.Handle
newHandle config = RD.Handle $ incomingTripsForStop' config

incomingTripsForStop' :: ObaRestConfig -> RD.StopID -> IO RDT.Stop
incomingTripsForStop' config stopId = do
  datas <- jsonForRouteAndParams config "arrivals-and-departures-for-stop"
             (T.unpack stopId) []
  case datas of
    Left err -> throwIO $ RD.RealtimeDataException err
    Right value -> do
      let maybeStop = parseMaybe parseJSON value
      case maybeStop of
        Just (ObaStop stop) -> return stop
        Nothing -> throwIO $ RD.RealtimeDataException "couldn't decode JSON from OBA"

-- | The OBA REST API methods are basically foo.bar/baz/METHOD/ID.json?params=things
-- This function takes care of putting that behind a reasonably usable interface, and
-- will result in `Either` an error `String` or an Aeson Object.
jsonForRouteAndParams :: 
  ObaRestConfig -> String -> String -> [(String, String)] -> IO (Either String Value)
jsonForRouteAndParams (ObaRestConfig key root) method id params = do
  result <- simpleHTTP (getRequest url)
  return $ case result of
    Left err -> Left $ show err
    Right (Response code reason _ body) -> 
      case code of
        (2, _, _) -> let decoded = decode $ pack $ map (toEnum . fromEnum) body
                     in  case decoded of
                           Just dec -> Right dec
                           Nothing -> Left "couldn't decode JSON!"
        _         -> Left reason
  where
    url = root ++ method ++ "/" ++ id ++ ".json?" ++ paramStr
    paramStr = 
      concat $ intersperse "&" $ map (\(k, v) -> k ++ "=" ++ v) (("key", key) : params)
