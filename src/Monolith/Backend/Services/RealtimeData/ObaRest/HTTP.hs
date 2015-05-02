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

{-# LANGUAGE DeriveDataTypeable #-}

module Monolith.Backend.Services.RealtimeData.ObaRest.HTTP
  ( jsonForRouteAndParams
  ) where

import Control.Exception
import Network.HTTP
import Data.Typeable
import Data.List (intersperse)
import Data.ByteString.Lazy (pack)
import Data.Aeson
import Monolith.Backend.Services.RealtimeData.ObaRest.Config

data HTTPException = HTTPException !String deriving (Show, Typeable)
instance Exception HTTPException

-- | The OBA REST API methods are basically foo.bar/baz/METHOD/ID.json?params=things
-- This function takes care of putting that behind a reasonably usable interface, and
-- will result in `Either` an error `String` or an Aeson FromJSON instance.
jsonForRouteAndParams :: FromJSON j =>
  ObaRestConfig -> String -> Maybe String -> [(String, String)] -> IO j

jsonForRouteAndParams (ObaRestConfig key root) method maybeId params = do
  let throwErr = throwIO . HTTPException
  result <- simpleHTTP (getRequest url)
  case result of
    Left err -> throwErr $ show err
    Right (Response code reason _ body) ->
      case code of
        (2, _, _) -> let decoded = decode $ pack $ map (toEnum . fromEnum) body
                     in  case decoded of
                           Just dec -> return dec
                           Nothing -> throwErr "couldn't decode JSON!"
        _         -> throwErr $ show reason
  where
    url = case maybeId of
            Just id -> root ++ method ++ "/" ++ id ++ ".json?" ++ paramStr
            Nothing -> root ++ method ++ ".json?" ++ paramStr
    paramStr =
      concat $ intersperse "&" $ map (\(k, v) -> k ++ "=" ++ v) (("key", key) : params)
