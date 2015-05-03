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

module Monolith.Backend.Services.StaticData.ObaRest where

import Control.Exception
import Control.Monad (guard, forM)
import qualified Control.Concurrent.ReadWriteVar as RWV
import Data.List (find, filter, sortOn)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Data.Geo.Coordinate (Coordinate, (<°>))
import Data.Geo.Geodetic.Haversine (haversineD)

import Control.Lens (over, set, (^.), (^?), (^..))
import Data.Maybe (maybe)
import Data.Aeson
import Data.Aeson.Lens

import Monolith.Utility.Cache
import Monolith.Backend.Services.StaticData
import Monolith.Backend.Services.StaticData.ObaRest.Types
import Monolith.Backend.Services.RealtimeData.ObaRest.HTTP
import Monolith.Backend.Services.RealtimeData.ObaRest.Config

newHandle :: ObaRestConfig -> IO StaticData
newHandle config =
  StaticData <$> stopsWithinRadius' config
             <*> stopsWithinRadiusOfStop' config

-- | Helper function for both the methods this service offers
getStops :: ObaRestConfig -> (Double, Double, Double) -> IO [Stop]
getStops config (lon, lat, radius) =
  let params = [("lon", show lon), ("lat", show lat), ("radius", show radius)]
  in  map stopFrom <$> jsonForRouteAndParams config "stops-for-location" Nothing params

-- | We're building in a perma-cache here because this is in theory static data
-- that will only change once a month or even less often. This whole module
-- is a hack that will go away when we get GTFS data integrated, so don't
-- worry too much about the implications of this. In any case, it shouldn't
-- take up too much memory in normal operation.
stopsWithinRadius' :: ObaRestConfig -> IO (Point -> Double -> IO [Stop])
stopsWithinRadius' config = do
  cache <- newCache (getStops config) :: IO (HashCache (Double, Double, Double) [Stop])
  return $ \(Point lon lat) radius -> cacheLookup (lon, lat, radius) cache

-- | This function searches for a stop by ID and then returns information
-- about what's in the vicinity. It's intended to be used to populate a map
-- view nearby the kiosk or similar.
stopsWithinRadiusOfStop' :: ObaRestConfig
                         -> IO (StopId -> Double -> IO StopVicinity)
stopsWithinRadiusOfStop' config = do
  cache <-
    newCache (stopsWithinRadiusOfStop'' config) :: IO (HashCache (StopId, Double) StopVicinity)
  return $ \stopId radius -> cacheLookup (stopId, radius) cache

stopsWithinRadiusOfStop'' :: ObaRestConfig -> (StopId, Double) -> IO StopVicinity
stopsWithinRadiusOfStop'' config (stopId, radius) = do
  let throwErr = throw . StaticDataException
  value <-
    jsonForRouteAndParams  config "stop" (Just (T.unpack stopId)) [("radius", show radius)] :: IO Value

  (lon, lat) <- maybe (throwErr "could not parse stop data") return $ do
    code <- value ^? key "code" . _Integer
    guard $ code == 200

    [lon, lat] <- forM ["lon", "lat"] $ \keyStr ->
      value ^? key "data" . key "entry" . key keyStr . _Double
    return (lon, lat)

  stops <- getStops config (lon, lat, radius)

  let stopPred = (stopId ==) . _stopId
      maybeThisStop = find stopPred stops
      restOfStops = filter (not . stopPred) stops
      thisStop = case maybeThisStop of
        Just ts -> ts
        Nothing -> throwErr "unable to locate requested stop"

      coordErr = throwErr "could not compute inter-stop distances; invalid coordinates?"

  stopsWithDistances <- maybe coordErr return $
    sortOn (^. stopDistanceFromHomeStop) <$>
      mapM (addDistanceToHomeStop thisStop) restOfStops

  return $ StopVicinity thisStop radius stopsWithDistances [] [] []

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
