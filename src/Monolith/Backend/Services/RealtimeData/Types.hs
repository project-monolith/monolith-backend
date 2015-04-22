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

-- | This module contains the types and instances that pertain directly to
-- the data served up by the REST API. They do not apply outside of that
-- context.
module Monolith.Backend.Services.RealtimeData.Types where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Aeson.TH as J

-- | This type encapsulates some information about a stop, and contains
-- a list of routes and upcoming trips for those routes. (and so on).
-- Since this stop data is tied to a certain time, the `stopTimestamp`
-- member records the time when this datum was generated.
--
-- We use `Set`s here so that there's no possibility for things to get
-- out of order. The correct ordering is important.
data Stop = Stop
  { stopId :: T.Text
  , stopDesc :: T.Text
  , stopRoutes :: [Route]
  , stopTimestamp :: Int -- timestamp
  } deriving Show

data Route = Route
  { routeId :: T.Text
  , earliestTrip :: Maybe Int -- timestamp
  , routeNumber :: T.Text
  , routeDesc :: T.Text
  , routeTrips :: S.Set Trip
  } deriving (Eq, Show)

data Trip = Trip
  { tripArrival :: Int -- timestamp
  , tripId :: T.Text
  , tripRouteId :: T.Text
  , tripWaitSource :: WaitSource
  , tripHeadSign :: T.Text
  } deriving (Eq, Ord, Show)

data WaitSource = Realtime | Scheduled deriving (Eq, Ord, Show)

$(J.deriveToJSON J.defaultOptions ''WaitSource)
$(J.deriveToJSON J.defaultOptions ''Trip)
$(J.deriveToJSON J.defaultOptions ''Route)
$(J.deriveToJSON J.defaultOptions ''Stop)
