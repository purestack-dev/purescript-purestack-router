module Bun.Request
  ( Request
  , method
  , url
  , json
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json)
import Effect.Aff (Aff)

foreign import data Request :: Type

foreign import jsonPromise :: Request -> Promise Json

json :: Request -> Aff Json
json = jsonPromise >>> Promise.toAff

foreign import method :: Request -> String

foreign import url :: Request -> String
