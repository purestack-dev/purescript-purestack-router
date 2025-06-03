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
  , "maybe"
  , "prelude"
  , "record"
  , "run"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "url-immutable"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
