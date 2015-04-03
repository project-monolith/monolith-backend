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

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- | This service implements the `Monolith.Backend.RealtimeData` interface.
-- It fetches data from the One Bus Away REST API.
module Monolith.Backend.Services.RealtimeData.ObaRest 
  ( ObaRestConfig(..)
  , newHandle
  , module RD
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Aeson.TH as J
import qualified Monolith.Backend.Services.RealtimeData as RD
import qualified Monolith.Backend.Services.RealtimeData.Types as RDT

data ObaRestConfig = ObaRestConfig
  { obaRestApiKey :: T.Text
  , obaRestRootUrl :: T.Text
  } deriving Show

$(J.deriveJSON J.defaultOptions ''ObaRestConfig)

newHandle :: ObaRestConfig -> IO RD.Handle
newHandle config = return handle
  where
    handle = RD.Handle $ \stopId duration -> do
      return $ RDT.Stop 0 "Foo" S.empty 0
