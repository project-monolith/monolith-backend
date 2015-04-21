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

module Monolith.Backend.Services.API.Scotty
  ( getHandle
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Web.Scotty
import Network.Wai.Handler.Warp (Port)
import Monolith.Backend.Services.API
import qualified Monolith.Backend.Services.RealtimeData as RD

getHandle :: RD.Handle -> Port -> IO Handle
getHandle dataService port = Handle <$> async (scotty port (app dataService))
  
app :: RD.Handle -> ScottyM ()
app dataService = do
  
  get "/stops/:stop_id/trips" $ do
    stopId <- param "stop_id"
    stop <- liftIO $ RD.incomingTripsForStop dataService stopId
    json stop 
