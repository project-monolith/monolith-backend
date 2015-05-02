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
    DeriveDataTypeable,
    FlexibleInstances,
    OverloadedStrings,
    TupleSections #-}

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
import Control.Applicative
import Control.Monad (guard, forM)
import Control.Lens (over, (^.), (^?), (^..))
import Data.Typeable
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.HashMap as HM
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

$(JTH.deriveToJSON JTH.defaultOptions { JTH.fieldLabelModifier = tail } ''Point)
makeLenses ''Point

type Route = T.Text

data Stop = Stop
  { _stopId :: !T.Text
  , _stopName :: !T.Text
  , _stopLocation :: !Point
  , _stopDirection :: !T.Text
  , _stopRoutes :: [Route]
  } deriving Show

$(JTH.deriveToJSON JTH.defaultOptions { JTH.fieldLabelModifier = tail } ''Stop)
makeLenses ''Stop

instance FromJSON [Stop] where
  parseJSON value = maybe empty return $ do
    -- get and test result status code
    code <- value ^? key "code" . _Integer
    guard $ code == 200

    -- we don't care about the timestamp because this is "static data"

    -- grab a list of the stops that came back from OBA
    stopValues <- return $ value ^.. key "data" . key "list" . values

    stops <- forM stopValues $ \stop -> do
      myId <- stop ^? key "id" . _String
      myName <- stop ^? key "name" . _String
      myDirection <- stop ^? key "direction" . _String

      [lon, lat] <- forM ["lon", "lat"] $ \myKey -> do
        stop ^? key myKey . _Double
      let point = Point lon lat

      routeIds <- return $ stop ^.. key "routeIds" . values . _String
      return $ Stop myId myName point myDirection routeIds

    -- now we need to change the route IDs to route "short names", aka
    -- the actual route numbers/names that we care about.

    routes <-
      return $ value ^.. key "data" . key "references" . key "routes" . values

    let routeNameMap = HM.fromList $ map idNameTuple routes
        throwErr = throw $ StaticDataException "bad route data from OBA"
        idNameTuple rv = maybe throwErr id $
          (,) <$> rv ^? key "id" . _String <*> rv ^? key "shortName" . _String

        -- map the stops with route IDs to stops with route *short names*.
        -- we are going to throw an error if ary data are missing.
        stops' = map convertRouteIds stops
        convertRouteIds = over stopRoutes $ \srs ->
          let nameForId rid = HM.lookup rid routeNameMap
              namesForIds = mapM nameForId srs
          in  case namesForIds of
                Just ns -> ns
                Nothing -> throwErr

    return stops'
