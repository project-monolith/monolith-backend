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
module Monolith.Services.RealtimeData.Types
  ( Stop (Stop)
  , stopId
  , stopDesc
  , stopRoutes
  , stopTimestamp

  , Route (Route)
  , routeId
  , routeNumber
  , routeDesc
  , routeTrips

  , TripWaitTime(..)

  , Trip (Trip)
  , tripArrival
  , tripWaitTime
  , tripId
  , tripRouteId
  , tripWaitSource
  , tripHeadSign

  , WaitSource (Realtime, Scheduled)
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson.TH as J
import qualified Control.Lens.TH as L

-- | This type encapsulates some information about a stop, and contains
-- a list of routes and upcoming trips for those routes. (and so on).
-- Since this stop data is tied to a certain time, the `stopTimestamp`
-- member records the time when this datum was generated.
--
-- We use `Set`s here so that there's no possibility for things to get
-- out of order. The correct ordering is important.
data Stop = Stop
  { _stopId :: !T.Text
  , _stopDesc :: !T.Text
  , _stopRoutes :: ![Route]
  , _stopTimestamp :: !Int -- timestamp
  } deriving Show

data Route = Route
  { _routeId :: !T.Text
  , _routeNumber :: !T.Text
  , _routeDesc :: !T.Text
  , _routeTrips :: !(S.Set Trip)
  } deriving (Eq, Show)

data TripWaitTime = TripDue Int | TripArrivesIn Int deriving (Eq, Ord, Show)

instance ToJSON TripWaitTime where
  toJSON (TripDue _) = String "DUE"
  toJSON (TripArrivesIn mins) = Number $ fromIntegral mins

data Trip = Trip
  { _tripArrival :: !Int -- timestamp
  , _tripWaitTime :: !(Maybe TripWaitTime) -- minutes
  , _tripId :: !T.Text
  , _tripRouteId :: !T.Text
  , _tripWaitSource :: !WaitSource
  , _tripHeadSign :: !T.Text
  } deriving (Eq, Ord, Show)

data WaitSource = Realtime | Scheduled deriving (Eq, Ord, Show)

jsonOptions = J.defaultOptions { J.fieldLabelModifier = tail }

$(J.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''WaitSource)
$(J.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''Trip)
$(J.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''Route)
$(J.deriveToJSON J.defaultOptions { J.fieldLabelModifier = tail } ''Stop)

L.makeLenses ''Stop
L.makeLenses ''Route
L.makeLenses ''Trip
