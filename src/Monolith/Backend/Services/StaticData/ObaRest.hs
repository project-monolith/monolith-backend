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

module Monolith.Backend.Services.StaticData.ObaRest where

import Control.Exception
import qualified Control.Concurrent.ReadWriteVar as RWV
import qualified Data.HashMap.Strict as HM
import Monolith.Utility.Cache
import Monolith.Backend.Services.StaticData
import Monolith.Backend.Services.StaticData.ObaRest.Types
import Monolith.Backend.Services.RealtimeData.ObaRest.HTTP
import Monolith.Backend.Services.RealtimeData.ObaRest.Config

newHandle :: ObaRestConfig -> IO StaticData
newHandle config = StaticData <$> stopsWithinRadius' config

-- | We're building in a perma-cache here because this is in theory static data
-- that will only change once a month or even less often. This whole module
-- is a hack that will go away when we get GTFS data integrated, so don't
-- worry too much about the implications of this. In any case, it shouldn't
-- take up too much memory in normal operation.
stopsWithinRadius' :: ObaRestConfig -> IO (Point -> Double -> IO [Stop])
stopsWithinRadius' config = do
  let getStops :: (Double, Double, Double) -> IO [Stop]
      getStops (lon, lat, radius) =
        let params = [("lon", show lon), ("lat", show lat), ("radius", show radius)]
        in  map stopFrom <$> jsonForRouteAndParams config "stops-for-location" Nothing params

  cache <- newCache getStops :: IO (HashCache (Double, Double, Double) [Stop])

  return $ \(Point lon lat) radius -> cacheLookup (lon, lat, radius) cache
