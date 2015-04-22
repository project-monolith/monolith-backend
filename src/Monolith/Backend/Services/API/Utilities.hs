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

-- | This module contains things that may be useful for any service that
-- implements the "Monolith.Backend.Services.API" interface, like (for
-- instance) the function `getTickerText` which turns 'Route's into text
-- for the ticker.
module Monolith.Backend.Services.API.Utilities
  ( getTickerText
  ) where

import Control.Lens ((^.))
import qualified Data.Foldable as F
import Data.List
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Monolith.Backend.Services.RealtimeData.Types

getTickerText :: UTCTime -> [Route] -> String
getTickerText now = intercalate " " . map toTickerItem
  where
    toTickerItem route =
      let trips = route ^. routeTrips
          waitForTimestamp t =
            (round $ diffUTCTime (posixSecondsToUTCTime $ fromIntegral t) now) `div` 60
          waits = map (waitForTimestamp . (^. tripArrival)) $ F.toList trips
          routeNum = route ^. routeNumber
      in  "Route " ++ T.unpack routeNum ++ " in " ++
            intercalate ", " (map show waits) ++ " mins."
