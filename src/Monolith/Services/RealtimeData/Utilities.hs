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

module Monolith.Services.RealtimeData.Utilities
  ( incomingTripsNearLocationImpl
) where

import Control.Lens ((^.))
import qualified Monolith.Services.RealtimeData as RD
import qualified Monolith.Services.RealtimeData.Types as RDT
import qualified Monolith.Services.StaticData as SD

incomingTripsNearLocationImpl :: SD.StaticData
                              -> (RD.StopID -> IO RDT.Stop)
                              -> (Double, Double)
                              -> Double
                              -> IO [RDT.Stop]
incomingTripsNearLocationImpl static stopGetter (long, lat) radius = do
  let point = SD.Point long lat
  staticStops <- SD.stopsWithinRadius static point radius
  let stopIds = map (^. SD.stopId) staticStops
  mapM stopGetter stopIds
