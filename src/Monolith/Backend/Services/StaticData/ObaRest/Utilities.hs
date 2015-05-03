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

module Monolith.Backend.Services.StaticData.ObaRest.Utilities
  ( getStops
  , getStop
  , makeStop
  , fillStopRoutes
  , addDistanceToHomeStop
  ) where

import Control.Exception (throwIO)
import Control.Monad (guard, join)
import Control.Lens (view, set, (^.), (^?), (^..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Aeson.Lens
import Data.Geo.Coordinate (Coordinate, (<°>))
import Data.Geo.Geodetic.Haversine (haversineD)

import Monolith.Backend.Services.StaticData
import Monolith.Backend.Services.RealtimeData.ObaRest.HTTP
import Monolith.Backend.Services.RealtimeData.ObaRest.Config

-- | Get a list of stops from the OBA API for the given lon, lat, and radius.
getStops :: ObaRestConfig -> (Double, Double, Double) -> IO [Stop]
getStops config (lon, lat, radius) = do
  let params = [("lon", show lon), ("lat", show lat), ("radius", show radius)]
  value <- jsonForRouteAndParams config "stops-for-location" Nothing params :: IO Value

  maybe (throwIO $ StaticDataException "failed to get stop data in getStops") return $ do
    code <- value ^? key "code" . _Integer
    guard $ code == 200
    stopValues <- return $ value ^.. key "data" . key "list" . values
    routesValue <- value ^? key "data" . key "references" . key "routes"
    mapM (flip makeStop routesValue) stopValues

-- | Get a `Stop` for the given ID from the OBA API.
getStop :: ObaRestConfig -> StopId -> IO Stop
getStop config stopId = do
  value <- jsonForRouteAndParams config "stop" (Just (T.unpack stopId)) [] :: IO Value

  maybe (throwIO $ StaticDataException "failed to get stop data in getStop") return $ do
    code <- value ^? key "code" . _Integer
    guard $ code == 200
    stopValue <- value ^? key "data" . key "entry"
    routesValue <- value ^? key "data" . key "references" . key "routes"
    makeStop stopValue routesValue

-- | with a "stop blob" `Value` and a "routes blob" `Value`, make a new `Stop`.
makeStop :: Value -> Value -> Maybe Stop
makeStop stopValue routesValue = do
  location <- Point <$> stopValue ^? key "lon" . _Double
                    <*> stopValue ^? key "lat" . _Double

  stop <- Stop <$> stopValue ^? key "id" . _String
               <*> stopValue ^? key "name" . _String
               <*> return location
               <*> stopValue ^? key "direction" . _String
               <*> return (stopValue ^.. key "routeIds" . values . _String)
               <*> return Nothing

  fillStopRoutes stop routesValue

-- | For a given stop and a `Value` containing a list of OBA route "references",
-- replace route ids in the stop's routes attribute with the route names that
-- correspond to those IDs.
fillStopRoutes :: Stop -> Value -> Maybe Stop
fillStopRoutes stop routeRefs = do
  routeNameMap <- HM.fromList <$> mapM idNameTuple (routeRefs ^.. values)
  let routeIds = view stopRoutes stop
  routeNames <- mapM (flip HM.lookup routeNameMap) routeIds
  return $ set stopRoutes routeNames stop
  where
    idNameTuple rv =
      (,) <$> rv ^? key "id" . _String <*> rv ^? key "shortName" . _String

-- | This function is used to compute the distance between two `Stop`s, and
-- to annotate one of the `Stop` arguments with the result.
addDistanceToHomeStop :: Stop -> Stop -> Maybe Stop
addDistanceToHomeStop home this = do
  homeC <- pointToCoordinate $ home ^. stopLocation
  thisC <- pointToCoordinate $ this ^. stopLocation
  let dist = haversineD homeC thisC
  return $ set stopDistanceFromHomeStop (Just dist) this

  where
    pointToCoordinate :: Point -> Maybe Coordinate
    pointToCoordinate point =
      let lon = point ^. pointLon
          lat = point ^. pointLat
      in  lat <°> lon
