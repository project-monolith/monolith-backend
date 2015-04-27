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

-- | This service implements the `Monolith.Backend.Services.RealtimeData` 
-- interface. It fetches data from the One Bus Away REST API.
module Monolith.Backend.Services.RealtimeData.ObaRest 
  ( newHandle
  , module Monolith.Backend.Services.RealtimeData.ObaRest.Config
  ) where

import Control.Exception
import qualified Data.Text as T
import qualified Monolith.Backend.Services.RealtimeData as RD
import qualified Monolith.Backend.Services.RealtimeData.Types as RDT
import Monolith.Backend.Services.RealtimeData.ObaRest.Types
import Monolith.Backend.Services.RealtimeData.ObaRest.HTTP
import Monolith.Backend.Services.RealtimeData.ObaRest.Config

-- * Get a new handle

newHandle :: ObaRestConfig -> RD.RealtimeData
newHandle config = RD.RealtimeData $ incomingTripsForStop' config

incomingTripsForStop' :: ObaRestConfig -> RD.StopID -> IO RDT.Stop
incomingTripsForStop' config stopId = do
  datas <- jsonForRouteAndParams config "arrivals-and-departures-for-stop"
             (Just $ T.unpack stopId) []
  case datas of
    Left err -> throwIO $ RD.RealtimeDataException err
    Right (ObaStop s) -> return s
