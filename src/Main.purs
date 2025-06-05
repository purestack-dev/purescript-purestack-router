module Main where

import Prelude

import Bun.Server as Bun
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import PureStack.Route (type (/), GET, POST)
import PureStack.Route as Router
import PureStack.Server as Server
import Run (AFF, Run(..), runBaseAff)
import Type.Row (type (+))

type Foo = { a :: String, b :: Maybe Int, c :: Array Number }

type API =
  ( foo :: "foo" / "bar" / Foo / GET { a :: Int, b :: Number, c :: Foo }
  , bar :: "foo" / "qux" / POST Unit { a :: Int }
  , deep ::
      "deep" /
        ( something :: "something" / GET { a :: String }
        )
  )

main :: Effect Unit
main = do
  log "🍝"
  Bun.serve $ Server.run @API identity
    { foo: \x -> pure { a: 1, b: 1.0, c: x }
    , bar: pure { a: 8 }
    , deep: { something: pure { a: "something" } }
    }
