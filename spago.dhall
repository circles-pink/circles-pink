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
  , "argonaut-core"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "bifunctors"
  , "checked-exceptions"
  , "codec-argonaut"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "http-methods"
  , "identity"
  , "maybe"
  , "milkis"
  , "monad-unlift"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "optparse"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "promises"
  , "psci-support"
  , "stadium"
  , "strings"
  , "subtlecrypto"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-lists"
  , "typelevel-prelude"
  , "undefined"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "./pkgs/purs/src/**/*.purs", "./pkgs/purs/test/**/*.purs" ]
}
