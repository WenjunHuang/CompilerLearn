{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "console"
  , "dts"
  , "effect"
  , "either"
  , "integers"
  , "labeled-data"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "transformers"
  , "tuples"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
