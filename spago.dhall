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
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "checked-exceptions"
  , "console"
  , "debug"
  , "dot-language"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "http-methods"
  , "identity"
  , "maybe"
  , "milkis"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "optparse"
  , "ordered-collections"
  , "prelude"
  , "promises"
  , "psci-support"
  , "stadium"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-lists"
  , "typelevel-prelude"
  , "undefined"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "./pkgs/purs/src/**/*.purs", "./pkgs/purs/test/**/*.purs" ]
}
