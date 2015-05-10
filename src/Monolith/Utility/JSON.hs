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

-- | Utility stuff for dealing with JSON data
module Monolith.Utility.JSON
  ( fixFieldName
  ) where

import Data.Char (isLower, toLower)
import Data.List (dropWhile, span)

-- | Given a fieldname like _routeEarliestArrival, return earliestArrival
fixFieldName :: String -> String
fixFieldName str =
  case rest of
    "" -> first
    (c:cs) -> toLower c : cs
  where
    withoutUnderscore = dropWhile ('_' ==) str
    (first, rest) = span isLower withoutUnderscore
