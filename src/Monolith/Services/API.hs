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

-- | This module contains the interface for services which provide the actual
-- API functionality of the Monolith backend: running a web server and
-- responding to requests for various data.
module Monolith.Services.API
  ( API(..)
  ) where

import Control.Concurrent.Async (Async)

-- | Handle for an API service instance.
newtype API = API
  { -- | The 'Async' reference to the thread that started the API service.
    getAsync :: Async ()
  }
