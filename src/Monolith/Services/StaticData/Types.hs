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

{-# LANGUAGE
    TemplateHaskell,
    DeriveDataTypeable #-}

-- | This module contains outward-facing types relevant to the static data
-- service interface, which is basically responsible for providing info like
-- which stops are where, which routes service which stops, and so on.
module Monolith.Services.StaticData.Types
  ( -- * Exceptions
    StaticDataException(..)

    -- * Point data type and lenses
  , Point(..)
  , pointLon
  , pointLat

    -- * Stop data type and lenses
  , Route
  , StopId

  , Stop(..)
  , stopId
  , stopName
  , stopLocation
  , stopDirection
  , stopRoutes
  , stopDistance

    -- * Stop vicinity data type and lenses
  , StopVicinity(..)
  , vicinityHomeStop
  , vicinityRadius
  , vicinityNearbyStops
  , vicinityBikeShares
  , vicinityCarShares
  , vicinityEvents
  ) where

import Control.Exception
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Aeson.TH as JTH
import Control.Lens.TH (makeLenses)
import Monolith.Utility.JSON (fixFieldName)

data StaticDataException = StaticDataException !String deriving (Show, Typeable)
instance Exception StaticDataException

data Point = Point
  { _pointLon :: !Double
  , _pointLat :: !Double
  } deriving Show

$(JTH.deriveToJSON JTH.defaultOptions { JTH.fieldLabelModifier = fixFieldName } ''Point)
makeLenses ''Point

type Route = T.Text
type StopId = T.Text

data Stop = Stop
  { _stopId :: !StopId
  , _stopName :: !T.Text
  , _stopLocation :: !Point
  , _stopDirection :: !T.Text
  , _stopRoutes :: ![Route]
  , _stopDistance :: !(Maybe Double)
  } deriving Show

$(JTH.deriveToJSON JTH.defaultOptions { JTH.fieldLabelModifier = fixFieldName } ''Stop)
makeLenses ''Stop

data StopVicinity = StopVicinity
  { _vicinityHomeStop :: Stop
  , _vicinityRadius :: Double
  , _vicinityNearbyStops :: [Stop]
  , _vicinityBikeShares :: [()]
  , _vicinityCarShares :: [()]
  , _vicinityEvents :: [()]
  } deriving Show

$(JTH.deriveToJSON JTH.defaultOptions { JTH.fieldLabelModifier = fixFieldName } ''StopVicinity)
makeLenses ''StopVicinity
