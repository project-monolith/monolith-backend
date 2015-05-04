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

-- | This service implements the `Monolith.Services.RealtimeData`
-- interface. It sits on another implementation of the same interface, and
-- acts as a caching layer that allows previously computed results within
-- some staleness threshold to be re-used.
module Monolith.Services.RealtimeData.Cache
  ( CacheConfig(..)
  , defaultCacheConfig
  , newRealtimeCache
  ) where

import Control.Applicative
import Control.Monad
import qualified Control.Concurrent.ReadWriteVar as RWV
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Monolith.Services.RealtimeData
import Monolith.Services.RealtimeData.Types

-- | Configuration type for this service
data CacheConfig = CacheConfig
  { -- | The expiration time of cache data, in seconds
    expirationTime :: !Int
  } deriving Show

$(deriveJSON defaultOptions ''CacheConfig)

defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig { expirationTime = 60 }

-- | Create a new Cache service wrapping another realtime data service.
newRealtimeCache :: CacheConfig -> RealtimeData -> IO RealtimeData
newRealtimeCache (CacheConfig expTime) dataService =
  RealtimeData <$> incomingTripsForStop' expTime dataService

type RWMap = RWV.RWVar (HM.HashMap StopID (UTCTime, Stop))

incomingTripsForStop' :: Int -> RealtimeData -> IO (StopID -> IO Stop)
incomingTripsForStop' expTime dataService = do
  rwMap <- RWV.new HM.empty :: IO RWMap

  return $ \stopId -> do
    maybeCached <- RWV.with rwMap (return . HM.lookup stopId)
    now <- getCurrentTime

    let refreshAction = refresh rwMap stopId dataService
    case maybeCached of
      Just (time, stop) ->
        let age = round $ diffUTCTime now time
        in  if age > expTime
            then putStrLn "found stale data, refreshing" >> refreshAction
            else putStrLn "found fresh data" >> return stop
      Nothing -> putStrLn "found no data, loading fresh" >> refreshAction

refresh :: RWMap -> StopID -> RealtimeData -> IO Stop
refresh rwMap stopId dataService = do
  stop <- incomingTripsForStop dataService stopId
  now <- getCurrentTime
  RWV.modify_ rwMap (return . HM.insert stopId (now, stop))
  return stop
