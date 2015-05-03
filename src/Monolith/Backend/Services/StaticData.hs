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

-- | This is the interface for modules that provide static transit service
-- information to other services in the Monolith backend. Note that services
-- implementing this interface can cheat, and use whatever data sources
-- they want...
--
-- As we develop it, this interface should basically be a layer on top of
-- GTFS (static) or equivalent; it should provide static data concerning the
-- transit infrastructure.
module Monolith.Backend.Services.StaticData
  ( StaticData(..)
  , module Monolith.Backend.Services.StaticData.Types
  ) where

import Monolith.Backend.Services.StaticData.Types

data StaticData = StaticData
  { getStopForId :: StopId -> IO Stop
  , stopsWithinRadius :: Point -> Double -> IO [Stop]
  , stopsWithinRadiusOfStop :: StopId -> Double -> IO StopVicinity }
