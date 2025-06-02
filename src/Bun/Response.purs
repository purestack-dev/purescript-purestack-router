module Bun.Response where

import Data.Argonaut (Json)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)

foreign import data Response :: Type

type Options = { status :: Int, statusText :: String, headers :: Array (String /\ String) }

foreign import json :: Json -> Options -> Response
foreign import string :: String -> Options -> Response
