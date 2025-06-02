module Bun.Server (serve) where

import Prelude

import Bun.Request (Request)
import Bun.Response (Response)
import Effect (Effect)

foreign import serve :: (Request -> Effect Response) -> Effect Unit
