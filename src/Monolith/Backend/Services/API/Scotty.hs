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
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Web.Scotty
import Network.Wai.Handler.Warp (Port)
import Monolith.Backend.Services.API
import Monolith.Backend.Services.RealtimeData

-- | Create a new 'API'. This kicks off a new thread and starts up a
-- Scotty in it. Presumably events coming into this web server will drive
-- the rest of this application; all services should be written with
-- concurrency in mind.
getHandle :: RealtimeData -> Port -> IO API
getHandle dataService port = API <$> async (scotty port (app dataService))
  
app :: RealtimeData -> ScottyM ()
app dataService = do
  
  get "/stops/:stop_id/trips" $ do
    stopId <- param "stop_id"
    stop <- liftIO $ incomingTripsForStop dataService stopId
    json stop 
