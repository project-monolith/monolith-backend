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
import Monolith.Backend.Services.StaticData
import Monolith.Backend.Services.RealtimeData.ObaRest.HTTP
import Monolith.Backend.Services.RealtimeData.ObaRest.Config

newHandle :: ObaRestConfig -> StaticData
newHandle config = StaticData $ stopsWithinRadius' config

stopsWithinRadius' :: ObaRestConfig -> Point -> Double -> IO [Stop]
stopsWithinRadius' config (Point lon lat) radius = do
  let params = [("lon", show lon), ("lat", show lat), ("radius", show radius)]
  datas <- jsonForRouteAndParams config "stops-for-location" Nothing params
  case datas of
    Left err -> throwIO $ StaticDataException err
    Right (Stop s) -> return s
