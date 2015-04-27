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

module Monolith.Backend.Services.StaticData.Types
  ( Point(..)
  , pointLon
  , pointLat

  , Stop(..)
  , stopId
  , stopName
  , stopLocation
  , stopDirection
  , stopRoutes
  ) where

import qualified Data.Text as T
import qualified Data.Aeson.TH as J
import Control.Lens.TH (makeLenses)

data Point = Point
  { _pointLon :: !Double
  , _pointLat :: !Double
  } deriving Show

$(J.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''Point)
makeLenses ''Point

type Route = T.Text

data Stop = Stop 
  { _stopId :: !T.Text
  , _stopName :: !T.Text
  , _stopLocation :: !Point
  , _stopDirection :: !T.Text
  , _stopRoutes :: [Route]
  } deriving Show

$(J.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''Stop)
makeLenses ''Stop
