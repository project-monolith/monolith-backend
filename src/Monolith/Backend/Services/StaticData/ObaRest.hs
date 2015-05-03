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
import Data.List (find, filter, sortOn)
import qualified Data.Text as T

import Control.Lens ((^.))
import Data.Maybe (maybe)
import Data.Aeson

import Monolith.Utility.Cache
import Monolith.Backend.Services.StaticData
import Monolith.Backend.Services.StaticData.ObaRest.Utilities
import Monolith.Backend.Services.RealtimeData.ObaRest.HTTP
import Monolith.Backend.Services.RealtimeData.ObaRest.Config

newHandle :: ObaRestConfig -> IO StaticData
newHandle config =
  StaticData <$> getStopForId' config
             <*> stopsWithinRadius' config
             <*> stopsWithinRadiusOfStop' config

getStopForId' :: ObaRestConfig -> IO (StopId -> IO Stop)
getStopForId' config = do
  cache <- newCache (getStop config) :: IO (HashCache StopId Stop)
  return $ flip cacheLookup cache

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
  value <- jsonForRouteAndParams config "stop" (Just (T.unpack stopId)) [] :: IO Value

  homeStop <- getStop config stopId
  let lon = homeStop ^. stopLocation ^. pointLon
      lat = homeStop ^. stopLocation ^. pointLat

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
