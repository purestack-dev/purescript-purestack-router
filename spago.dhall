{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "run"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "url-immutable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
