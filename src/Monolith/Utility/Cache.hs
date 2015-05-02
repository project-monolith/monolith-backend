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

{-# LANGUAGE MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances #-}

-- | This module contains the `Cache` type, which is basically a wrapper around
-- a read/write lock and some kind of map. These `Cache`s are oriented toward
-- caching the results of expensive `IO` actions.
module Monolith.Utility.Cache
  ( CacheMap
  , Cache
  , HashCache
  , newCache
  , cacheLookup
  ) where

import qualified Control.Concurrent.ReadWriteVar as RWV
import Data.Hashable
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM

-- | Typeclass for map structures that can be used in a Cache
class CacheMap m k v | m -> k, m -> v where
  empty :: m
  mapLookup :: k -> m -> Maybe v
  mapInsert :: k -> v -> m -> m

instance (Eq k, Hashable k) => CacheMap (HM.HashMap k v) k v where
  empty = HM.empty
  mapLookup = HM.lookup
  mapInsert = HM.insert

data Cache m k v = Cache
  { cacheDefault :: k -> IO v
  , cacheMap :: RWV.RWVar m }

type HashCache k v = Cache (HM.HashMap k v) k v

newCache :: CacheMap m k v => (k -> IO v) -> IO (Cache m k v)
newCache defaultFun = Cache defaultFun <$> RWV.new empty

-- | Lookup a key in a `Cache`, running the default function and caching its
-- result if the key is not known.
cacheLookup :: (CacheMap m k v) => k -> Cache m k v -> IO v
cacheLookup key (Cache defaultFun rwMap) = do
  maybeCached <- RWV.with rwMap (return . mapLookup key)
  case maybeCached of
    Just value -> putStrLn "found cached data" >> return value
    Nothing -> do
      putStrLn "loading new data"
      newValue <- defaultFun key
      RWV.modify_ rwMap (return . mapInsert key newValue)
      return newValue
