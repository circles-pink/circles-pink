{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "argonaut-core"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "http-methods"
  , "maybe"
  , "milkis"
  , "prelude"
  , "promises"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "./pkgs/purs/src/**/*.purs", "./pkgs/purs/test/**/*.purs" ]
}
