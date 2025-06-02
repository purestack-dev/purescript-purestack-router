module Bun.Request where

import Control.Promise (Promise)
import Data.Argonaut (Json)

foreign import data Request :: Type

foreign import json :: Request -> Promise Json

foreign import method :: Request -> String

foreign import url :: Request -> String
