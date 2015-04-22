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

{-# LANGUAGE OverloadedStrings #-}

module Monolith.Backend.Services.RealtimeData.ObaRest.Types
 ( ObaStop (..)
 ) where

import Control.Applicative
import Control.Monad (guard, forM)
import Control.Lens (set, (^.))
import Data.List
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Monolith.Backend.Services.RealtimeData.Types as RDT

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
        arrivalKey = case predicted of
                       RDT.Realtime -> "predictedArrivalTime"
                       RDT.Scheduled -> "scheduledArrivalTime"
        
    arrival <- (`div` 1000) <$> o .: arrivalKey
    headsign <- o .: "tripHeadsign"
    return $ ObaTrip $ RDT.Trip arrival Nothing tripId routeId predicted headsign
  parseJSON _ = empty

setTripWaitTime :: Int -- ^ The timestamp to use as "now"
                -> RDT.Trip -- ^ A trip with no (?) wait time
                -> RDT.Trip -- ^ the new trip with timestamp added
setTripWaitTime now t =
  let wait = round $ fromIntegral ((t ^. RDT.tripArrival) - now) / 60.0
  in  set RDT.tripWaitTime (Just wait) t

newtype ObaRoute = ObaRoute
  { obaGetRoute :: RDT.Route
  } deriving Show

instance FromJSON ObaRoute where
  parseJSON (Object o) =
    let route = RDT.Route <$> o .: "id" <*> return Nothing <*> 
                  o .: "shortName" <*> o .: "description" <*> return S.empty
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
    baseData <- getObject =<< getAttr "data" o
    entryData <- getObject =<< getAttr "entry" baseData

    tripsValue <- getAttr "arrivalsAndDepartures" entryData
    routesValue <- getAttr "routes" =<< getObject =<<
                     getAttr "references" baseData

    let getStopId (String s) = return s
        getStopId _ = empty
    stopId <- getStopId =<< getAttr "stopId" entryData

    -- now parse the lists of trips and routes
    trips' <- fmap obaGetTrip <$> parseJSON tripsValue :: Parser [RDT.Trip]
    routes <- fmap obaGetRoute <$> parseJSON routesValue :: Parser [RDT.Route]

    -- add wait times to trips
    let trips = map (setTripWaitTime timestamp) trips'

    let routeMap =
          foldr (\r@(RDT.Route routeId _ _ _ _) m -> HM.insert routeId r m) HM.empty routes

        f t@(RDT.Trip _ _ _ routeId _ _) m =
          let g (RDT.Route id earliest num desc trips) =
                let earliest' = case earliest of
                      Just e -> Just $ min e (t ^. RDT.tripArrival)
                      Nothing -> Just $ (t ^. RDT.tripArrival)
                in  RDT.Route id earliest' num desc (S.insert t trips)
          in  HM.adjust g routeId m

        routesWithTrips = foldr f routeMap trips
        routesWithNotNullTrips =
          filter (not . S.null . (^. RDT.routeTrips)) $ HM.elems routesWithTrips
        sortedRoutes = sortOn (^. RDT.earliestTrip) routesWithNotNullTrips

    return $ ObaStop $ RDT.Stop stopId "bar" sortedRoutes timestamp

getAttr :: T.Text -> Object -> Parser Value
getAttr key object = do
  value <- object .: key :: Parser Value
  return value

getObject :: Value -> Parser Object
getObject (Object o) = return o
getObject _ = empty
