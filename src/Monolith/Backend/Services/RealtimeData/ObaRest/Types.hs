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
import Control.Monad (guard)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
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
    return $ ObaTrip $ RDT.Trip arrival tripId routeId predicted headsign
  parseJSON _ = empty

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
    baseData <- getObject =<< getAttr "data" o

    tripsValue <- getAttr "arrivalsAndDepartures" =<<
                    getObject =<< getAttr "entry" baseData 

    routesValue <- getAttr "routes" =<< getObject =<<
                     getAttr "references" baseData

    -- now parse the lists of trips and routes
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

getAttr :: T.Text -> Object -> Parser Value
getAttr key object = do
  value <- object .: key :: Parser Value
  return value

getObject :: Value -> Parser Object
getObject (Object o) = return o
getObject _ = empty
