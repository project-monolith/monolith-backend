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

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import System.Environment
import Control.Concurrent.Async
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode)
import System.Remote.Monitoring (forkServer)

import qualified Monolith.Services.RealtimeData.ObaRest as OBA
import qualified Monolith.Services.RealtimeData.Cache as CH
import qualified Monolith.Services.StaticData.ObaRest as OBAS
import Monolith.Services.API (API(..))
import qualified Monolith.Services.API.Scotty as SCTY

config :: OBA.ObaRestConfig
config = OBA.ObaRestConfig "TEST" "http://api.pugetsound.onebusaway.org/api/where/"

main :: IO ()
main = do
  forkServer "localhost" 8000
  obas <- OBAS.newHandle config
  let oba = OBA.newHandle config obas
  cache <- CH.newRealtimeCache CH.defaultCacheConfig oba obas
  API async <- SCTY.getHandle cache obas 4567
  wait async
