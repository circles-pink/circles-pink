module PursDeps
  ( main
  , tests
  ) where

import Prelude
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Argonaut as AG
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, jobject)
import Data.Codec.Argonaut.Record (object)
import Data.Either (Either(..), hush)
import Data.Foldable (fold)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, case_, inj, on, onMatch)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import Foreign.Object (Object, toUnfoldable)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (exit)
import Options.Applicative (Parser, (<**>))
import Options.Applicative as O
import Prim.Row (class Cons, class Nub, class Union)
import Test.Unit as T
import Test.Unit.Assert as A
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Undefined (undefined)

--------------------------------------------------------------------------------
-- Opts
--------------------------------------------------------------------------------
type Opts
  = { depsJsonPath :: String
    }

parseOpts :: Parser Opts
parseOpts = ado
  depsJsonPath <-
    O.strOption
      $ fold
          [ O.long "depsJsonPath"
          -- , O.metavar "DEPS_JSON_PATH"
          , O.help "..."
          ]
  in { depsJsonPath }

main :: Effect Unit
main = do
  opts <- O.execParser prog
  r <- runResult { opts, cap } main'
  case r of
    Left e -> do
      error $ printError e
      exit 1
    Right _ -> exit 0
  where
  cap :: Cap Effect
  cap =
    { readFile:
        \p ->
          readTextFile UTF8 p
            # try
            <#> lmap (const $ inj (Proxy :: _ "errReadFile") p)
            # ExceptT
    , log: \s -> liftEffect $ log s
    }

  prog =
    O.info (parseOpts <**> O.helper)
      $ fold
          [ O.fullDesc
          , O.progDesc "Print a greeting for TARGET"
          , O.header "hello - a test for purescript-optparse"
          ]

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------
type Err r
  = (ErrReadFile + ErrParse + r)

type ErrReadFile r
  = ( errReadFile :: String | r )

type ErrParse r
  = ( errParse :: JsonDecodeError | r )

printError :: Variant (Err ()) -> String
printError =
  case_
    # on (Proxy :: _ "errReadFile") (\p -> "ERR_READ_FILE: \n" <> p)
    # on (Proxy :: _ "errParse") (\e -> "ERR_PARSE: \n" <> printJsonDecodeError e)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type PursDeps
  = Map String Json --{ path :: ModuleName, depends :: Array ModuleName }

type ModuleName
  = String

--------------------------------------------------------------------------------
-- Result
--------------------------------------------------------------------------------
type Result r env m a
  = ReaderT env (ExceptV (Err + r) m) a

runResult :: forall env m a. env -> Result () env m a -> m (Either (Variant (Err ())) a)
runResult cap r = runExceptT $ runReaderT r cap

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
type Cap m
  = { readFile :: forall r. String -> ExceptV (ErrReadFile + r) m String
    , log :: forall r. String -> ExceptV r m Unit
    }

codec :: Json -> Maybe PursDeps
codec j = do
  obj <- AG.toObject j
  let
    xs :: Array (String /\ Json)
    xs = toUnfoldable obj

    mp = fromFoldable xs
  pure mp

cd :: JsonCodec (Array (String /\ Json))
cd = jobject # pris

--c = objectOf (recordOf { path: cModuleName, depends: arrayOf cModuleName })
-- <#> (toUnfoldable >>> fromFoldable)
parse :: forall r. String -> Either (Variant (ErrParse + r)) PursDeps
parse s = undefined -- parseJson s >>= decodeJson # lmap (inj (Proxy :: _ "errParse"))

tests :: T.TestSuite
tests =
  T.suite "PursDeps" do
    T.test "parse" do
      let
        data_ :: String
        data_ =
          (stringify <<< encodeJson)
            { "Apple": { path: "", depends: [ "Foo.Bar" ] }
            , "Nut": { path: "", depends: [ "Foo.Bar" ] }
            }
      -- parsed :: PursDeps
      -- parsed =
      --   fromFoldable
      --     [ "Apple" /\ { path: "", depends: [ "Foo.Bar" ] }
      --     , "Nut" /\ { path: "", depends: [ "Foo.Bar" ] }
      --     ]
      A.equal 1 1

--A.equal (data_ # parse # hush) (Just parsed)
main' :: forall r' m r. Monad m => Result r { opts :: Opts, cap :: Cap m } m Unit
main' = do
  cap :: Cap m <- asks _.cap
  opts <- asks _.opts
  r <- lift $ cap.readFile opts.depsJsonPath
  res' <- lift $ except $ parse r
  let
    x = spy "res" res'
  lift $ cap.log "digraph mygraph { a1 -> a2; a2 -> a3; }"
  pure unit
