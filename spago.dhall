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
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "run"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "url-immutable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
