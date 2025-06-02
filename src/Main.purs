module Main where

import Prelude

import Bun.Server as Bun
import Effect (Effect)
import Effect.Console (log)
import PureStack.Router (type (/), GET, POST)
import PureStack.Router as Router

type API =
  ( foo :: "foo" / "bar" / "foo" / GET { a :: Int, b :: Number }
  , bar :: "foo" / "qux" / POST Unit { a :: Int }
  )

main :: Effect Unit
main = do
  log "🍝"
  Bun.serve $ Router.run @API { foo: pure { a: 1, b: 1.0 }, bar: pure { a: 8 } }
