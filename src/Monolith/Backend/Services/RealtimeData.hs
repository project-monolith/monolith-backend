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

{-# LANGUAGE DeriveDataTypeable #-}

-- | This is the interface for modules that provide realtime transit service
-- information to other services in the Monolith backend. This service is
-- specific to realtime data; static data should come from another service.
module Monolith.Backend.Services.RealtimeData
  ( RealtimeData(..)
  , RealtimeDataException(..)
  , StopID
  ) where

import Control.Exception
import Data.Typeable
import qualified Data.Text as T
import Monolith.Backend.Services.RealtimeData.Types

-- | General-purpose exception to throw when something goes wrong fetching
-- real-time data.
data RealtimeDataException = RealtimeDataException !String deriving (Show, Typeable)
instance Exception RealtimeDataException

type StopID = T.Text

-- | Handle for a real-time data service.
newtype RealtimeData = RealtimeData
  { -- | Get a 'Stop' object with real-time trip data for the given stop ID.
    incomingTripsForStop :: StopID -> IO Stop
  }
