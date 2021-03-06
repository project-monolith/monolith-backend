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

-- | This module implements the "Monolith.Services.API" interface with
-- Scotty.
module Monolith.Services.API.Scotty
  ( getHandle
  ) where

import Control.Applicative
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Lens (view, over, (^.))
import Control.Concurrent.Async
import Web.Scotty
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors (simpleCors)
import Data.List (sortOn)
import Data.Time.Clock (getCurrentTime)
import Monolith.Services.API
import Monolith.Services.API.Utilities
import Monolith.Services.RealtimeData
import Monolith.Services.RealtimeData.Types
import qualified Monolith.Services.StaticData as SD

-- | Create a new 'API'. This kicks off a new thread and starts up a
-- Scotty in it. Presumably events coming into this web server will drive
-- the rest of this application; all services should be written with
-- concurrency in mind.
getHandle :: RealtimeData -> SD.StaticData -> Port -> IO API
getHandle realtime static port =
  API <$> async (scotty port (app realtime static))

-- | The number of routes to return (and the number of trips to skip for the
-- ticker) by default.
routesCutoff :: Int
routesCutoff = 6

-- | The maximum number of trips to return per route. This applies to the trips
-- and ticker methods.
maxTrips :: Int
maxTrips = 3

-- | the default search radius for stops near location, in meters
stopSearchRadius :: Double
stopSearchRadius = 100.0

-- | The default minimum wait time, beyond which trips are dropped
minimumWaitTime :: Int
minimumWaitTime = -1

app :: RealtimeData -> SD.StaticData -> ScottyM ()
app realtime static = do
  middleware logStdoutDev
  middleware simpleCors

  get "/stops/:stop_id/trips" $ do
    stopId <- param "stop_id"
    nRoutes <- param "n_trips" `rescue` const (return routesCutoff)
    stop <- liftIO $ incomingTripsForStop realtime stopId
    json $ processStop nRoutes stop

  get "/stops/nearby" $ do
    lon <- param "longitude"
    lat <- param "latitude"
    radius <- param "radius" `rescue` const (return stopSearchRadius)    
    nRoutes <- param "n_trips" `rescue` const (return routesCutoff)
    stops <- liftIO $ incomingTripsNearLocation realtime (lon, lat) radius
    json $ map (processStop nRoutes) stops

  get "/stops/:stop_id/ticker" $ do
    stopId <- param "stop_id"
    nSkipRoutes <- param "n_skip_trips" `rescue` const (return routesCutoff)
    stop <- liftIO $ incomingTripsForStop realtime stopId

    let routes = drop nSkipRoutes $
                 sortByWaitTime $
                 map filterTrips $
                 view stopRoutes stop

        tickerText = getTickerText (stop ^. stopTimestamp) routes
    json tickerText

  get "/stops/:stop_id/vicinity" $ do
    stopId <- param "stop_id"
    radius <- param "radius" `rescue` const (return stopSearchRadius)
    json =<< liftIO (SD.stopsWithinRadiusOfStop static stopId radius)

  where
    processStop :: Int -> Stop -> Stop
    processStop nRoutes =
      over stopRoutes (take nRoutes . sortByWaitTime . filterRoutes . map filterTrips)

    filterRoutes :: [Route] -> [Route]
    filterRoutes = filter (not . null . (^. routeTrips))

    filterTrips :: Route -> Route
    filterTrips =
      let waitFor (TripDue w) = w
          waitFor (TripArrivesIn w) = w
          getWaitTime =
            waitFor . maybe (TripDue $ minimumWaitTime - 1) id . view tripWaitTime
      in  over routeTrips (take maxTrips . dropWhile ((minimumWaitTime >) . getWaitTime))

    -- | minimum is a partial function!!!!! oh noes!
    sortByWaitTime :: [Route] -> [Route]
    sortByWaitTime =
      sortOn (view tripWaitTime . minimum . view routeTrips)
