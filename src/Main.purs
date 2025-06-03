module Main where

import Prelude

import Bun.Server as Bun
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import PureStack.Route (type (/), GET, POST)
import PureStack.Route as Router
import Run (AFF, Run(..), runBaseAff)
import Type.Row (type (+))

type API =
  ( foo :: "foo" / "bar" / "foo" / GET { a :: Int, b :: Number }
  , bar :: "foo" / "qux" / POST Unit { a :: Int }
  )

x :: { foo :: Run (AFF + ()) { a :: Int, b :: Number }, bar :: Run (AFF + ()) { a :: Int } }
x = { foo: pure { a: 1, b: 1.0 }, bar: pure { a: 8 } }

main :: Effect Unit
main = do
  log "🍝"
-- Bun.serve $ Router.run @API 
