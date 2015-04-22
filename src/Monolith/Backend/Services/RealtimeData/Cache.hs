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

{-# LANGUAGE TemplateHaskell #-}

-- | This service implements the `Monolith.Backend.Services.RealtimeData`
-- interface. It sits on another implementation of the same interface, and
-- acts as a caching layer that allows previously computed results within
-- some staleness threshold to be re-used.
module Monolith.Backend.Services.RealtimeData.Cache 
  ( CacheConfig(..)
  , defaultCacheConfig
  ) where

import Control.Applicative
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Monolith.Backend.Services.RealtimeData
import Monolith.Backend.Services.RealtimeData.Types

-- | Configuration type for this service
data CacheConfig = CacheConfig
  { -- | The expiration time of cache data, in seconds
    expirationTime :: Int
  } deriving Show

$(deriveJSON defaultOptions ''CacheConfig)

defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig { expirationTime = 60 }

-- | Create a new Cache service wrapping another realtime data service.
newRealtimeCache :: CacheConfig -> RealtimeData -> IO RealtimeData
newRealtimeCache (CacheConfig expTime) dataService =
  RealtimeData <$> incomingTripsForStop' expTime dataService

incomingTripsForStop' :: Int -> RealtimeData -> IO (StopID -> IO Stop)
incomingTripsForStop' = undefined
