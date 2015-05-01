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

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Monolith.Backend.Services.StaticData.Types
  ( StaticDataException(..)

  , Point(..)
  , pointLon
  , pointLat

  , Stop(..)
  , stopId
  , stopName
  , stopLocation
  , stopDirection
  , stopRoutes
  ) where

import Control.Exception
import Control.Monad (guard, forM)
import Control.Lens (set, (^.), (^?))
import Data.Typeable
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Aeson.TH as JTH
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens.TH (makeLenses)

data StaticDataException = StaticDataException !String deriving (Show, Typeable)
instance Exception StaticDataException

data Point = Point
  { _pointLon :: !Double
  , _pointLat :: !Double
  } deriving Show

$(JTH.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''Point)
makeLenses ''Point

type Route = T.Text

data Stop = Stop
  { _stopId :: !T.Text
  , _stopName :: !T.Text
  , _stopLocation :: !Point
  , _stopDirection :: !T.Text
  , _stopRoutes :: [Route]
  } deriving Show

$(JTH.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''Stop)
makeLenses ''Stop

instance FromJSON [Stop] where
  parseJSON value = maybe empty return $ do
    -- get and test result status code
    code <- value ^? key "code" . _Integer
    guard $ code == 200

    -- we don't care about the timestamp because this is "static data"

    -- grab a list of the stops that came back from OBA
    stopValues <- value ^.. key "data" . key "list" . values

    stops <- forM stopValues $ \stop -> do
      myId <- stop ^? key "id" . _Text
      myName <- stop ^? key "name" . _Text
      myDirection <- stop ^? key "direction" . _Text

      [lon, lat] <- forM ["lon", "lat"] $ \myKey -> do
        stop ^? key myKey . _Double
      let point = Point lon lat

      routeIds <- stop ^? key "routeIds"
      
