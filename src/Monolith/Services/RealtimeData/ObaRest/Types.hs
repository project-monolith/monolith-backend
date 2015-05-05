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

module Monolith.Services.RealtimeData.ObaRest.Types
 ( ObaStop (..)
 ) where

import Control.Applicative
import Control.Monad (guard, forM)
import Control.Lens (set, view, over, (^.), (^?))
import Data.List
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.Aeson
import Data.Aeson.Lens
import qualified Monolith.Services.RealtimeData.Types as RDT

-- * Wrapper types for parsing the realtime data types without overlapping
-- instances

newtype ObaTrip = ObaTrip
  { obaGetTrip :: RDT.Trip
  } deriving Show

instance FromJSON ObaTrip where
  parseJSON (Object o) = do
    tripId <- o .: "tripId"
    routeId <- o .: "routeId"
    predictedB <- o .: "predicted"
    let predicted = if predictedB then RDT.Realtime else RDT.Scheduled
        arrivalKey = case predicted of
                       RDT.Realtime -> "predictedArrivalTime"
                       RDT.Scheduled -> "scheduledArrivalTime"

    arrival <- (`div` 1000) <$> o .: arrivalKey
    headsign <- o .: "tripHeadsign"
    return $ ObaTrip $ RDT.Trip arrival Nothing tripId routeId predicted headsign
  parseJSON _ = empty

-- | Dummy instance to allow lens magic
instance ToJSON ObaTrip where
  toJSON _ = Null

setTripWaitTime :: Int -- ^ The timestamp to use as "now"
                -> RDT.Trip -- ^ A trip with no (?) wait time
                -> RDT.Trip -- ^ the new trip with timestamp added
setTripWaitTime now t =
  let wait = round $ fromIntegral ((t ^. RDT.tripArrival) - now) / 60.0
      twt = if wait < 1 then RDT.TripDue wait else RDT.TripArrivesIn wait
  in  set RDT.tripWaitTime (Just twt) t

newtype ObaRoute = ObaRoute
  { obaGetRoute :: RDT.Route
  } deriving Show

instance FromJSON ObaRoute where
  parseJSON (Object o) =
    let route = RDT.Route <$> o .: "id" <*>
          o .: "shortName" <*> o .: "description" <*> return []
    in  ObaRoute <$> route

-- | Dummy instance to allow lens magic
instance ToJSON ObaRoute where
  toJSON _ = Null

newtype ObaStop = ObaStop RDT.Stop

-- * The main type that does the work of pulling apart the JSON blob from OBA
-- and turning it into our RealtimeData types

instance FromJSON ObaStop where
  parseJSON value = maybe empty return $ do
    -- get and test result status code
    code <- value ^? key "code" . _Integer
    guard $ code == 200

    -- get the timestamp from the returned data and convert it to seconds
    timestamp <- (`div` 1000) . fromInteger <$> value ^? key "currentTime" . _Integer

    -- grab the rest of the relevant data out of the JSON blob
    _data <- value ^? key "data"
    entry <- _data ^? key "entry"

    tripsValue <- entry ^? key "arrivalsAndDepartures" . _Value
    routesValue <- _data ^? key "references" . key "routes" . _Value

    stopId <- entry ^? key "stopId" . _String

    -- now parse the lists of trips and routes
    trips' <- fmap obaGetTrip <$> tripsValue ^? _JSON :: Maybe [RDT.Trip]
    routes <- fmap obaGetRoute <$> routesValue ^? _JSON :: Maybe [RDT.Route]

    -- add wait times to trips
    let trips = map (setTripWaitTime timestamp) trips'

    let routeMap =
          foldr (\r@(RDT.Route routeId _ _ _) m -> HM.insert routeId r m) HM.empty routes

        f t m = HM.adjust (over RDT.routeTrips (t :)) (view RDT.tripRouteId t) m

        routesWithTrips = foldr f routeMap trips
        sortTrips = map (over RDT.routeTrips (sortOn (^. RDT.tripArrival)))
        filterNullRoutes =
          filter (not . null . (^. RDT.routeTrips)) . HM.elems

        finalRoutes = sortTrips $ filterNullRoutes routesWithTrips

    return $ ObaStop $ RDT.Stop stopId "bar" finalRoutes timestamp
