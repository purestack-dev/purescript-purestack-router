{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "maybe"
  , "prelude"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "url-immutable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
