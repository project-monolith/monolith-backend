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

-- | This service implements the `Monolith.Services.RealtimeData`
-- interface. It fetches data from the One Bus Away REST API.
module Monolith.Services.RealtimeData.ObaRest
  ( newHandle
  , module Monolith.Services.RealtimeData.ObaRest.Config
  ) where

import Control.Exception
import Control.Lens (view, set)
import qualified Data.Text as T
import qualified Monolith.Services.RealtimeData as RD
import qualified Monolith.Services.RealtimeData.Types as RDT
import Monolith.Services.RealtimeData.ObaRest.Types
import Monolith.Services.RealtimeData.ObaRest.HTTP
import Monolith.Services.RealtimeData.ObaRest.Config
import qualified Monolith.Services.StaticData as SD

-- * Get a new handle

newHandle :: ObaRestConfig -> SD.StaticData -> RD.RealtimeData
newHandle config static = RD.RealtimeData $ incomingTripsForStop' config static

incomingTripsForStop' :: ObaRestConfig -> SD.StaticData -> RD.StopID -> IO RDT.Stop
incomingTripsForStop' config static stopId = do
  (ObaStop s) <- jsonForRouteAndParams config "arrivals-and-departures-for-stop"
    (Just $ T.unpack stopId) []

  myDesc <- view SD.stopName <$> SD.getStopForId static stopId
  return $ set RDT.stopDesc myDesc s
