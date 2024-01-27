{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "arrays"
  , "console"
  , "dts"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "labeled-data"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "transformers"
  , "tuples"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
