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

-- | This module implements the "Monolith.Backend.Services.API" interface with
-- Scotty.
module Monolith.Backend.Services.API.Scotty
  ( getHandle
  ) where

import Control.Applicative
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Lens (over, (^.))
import Control.Concurrent.Async
import Web.Scotty
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Time.Clock (getCurrentTime)
import Monolith.Backend.Services.API
import Monolith.Backend.Services.API.Utilities
import Monolith.Backend.Services.RealtimeData
import Monolith.Backend.Services.RealtimeData.Types
import qualified Monolith.Backend.Services.StaticData as SD

-- | Create a new 'API'. This kicks off a new thread and starts up a
-- Scotty in it. Presumably events coming into this web server will drive
-- the rest of this application; all services should be written with
-- concurrency in mind.
getHandle :: RealtimeData -> SD.StaticData -> Port -> IO API
getHandle realtime static port =
  API <$> async (scotty port (app realtime static))

-- | The number of trips to return (and the number of trips to skip for the
-- ticker) by default.
tripsCutoff :: Int
tripsCutoff = 9

-- | the default search radius for stops near location, in meters
stopSearchRadius :: Double
stopSearchRadius = 100.0

app :: RealtimeData -> SD.StaticData -> ScottyM ()
app realtime static = do
  middleware logStdoutDev

  get "/stops/:stop_id/trips" $ do
    stopId <- param "stop_id"
    nTrips <- param "n_trips" `rescue` const (return tripsCutoff)
    stop <- liftIO $ incomingTripsForStop realtime stopId
    let stop' = over stopRoutes (take nTrips) stop
    json stop'

  get "/stops/:stop_id/ticker" $ do
    stopId <- param "stop_id"
    nSkipTrips <- param "n_skip_trips" `rescue` const (return tripsCutoff)
    stop <- liftIO $ incomingTripsForStop realtime stopId
    let stop' = over stopRoutes (drop nSkipTrips) stop
    now <- liftIO $ getCurrentTime
    let tickerText = getTickerText now $ drop nSkipTrips $ stop ^. stopRoutes
    json tickerText

  get "/stops/near_location" $ do
    [lon, lat] <-
      forM ["lon", "lat"] $ \name -> read <$> param name
    radius <-
      param "radius" `rescue` const (return stopSearchRadius)

    json =<< liftIO (SD.stopsWithinRadius static (SD.Point lon lat) radius)
