module PursDeps
  ( main
  ) where

import Prelude
import Control.Monad.Except (ExceptT, except, lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Variant (Variant)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Process (exit)
import Options.Applicative (Parser, (<**>))
import Options.Applicative as O
import Prim.Row (class Cons, class Nub, class Union)
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
      log $ printError e
      exit 1
    Right _ -> exit 0
  where
  cap :: Cap Effect
  cap =
    { readFile: \_ -> pure "abc"
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
  = (ErrReadFile + ErrParse + ErrLog + r)

type ErrReadFile r
  = ( errReadFile :: String | r )

type ErrParse r
  = ( errParse :: String | r )

type ErrLog r
  = ( errLog :: String | r )

printError :: Variant (Err ()) -> String
printError _ = "error"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type PursDeps
  = Map String { path :: ModuleName, depends :: Array ModuleName }

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
    , log :: forall r. String -> ExceptV (ErrLog + r) m Unit
    }

parse :: forall r. String -> Either (Variant (ErrParse + r)) PursDeps
parse = undefined

main' :: forall r' m r. Monad m => Result r { opts :: Opts, cap :: Cap m } m Unit
main' = do
  cap :: Cap m <- asks _.cap
  opts <- asks _.opts
  r <- lift $ cap.readFile opts.depsJsonPath
  -- res' <- lift $ except $ parse r
  -- let
  --   x = spy "res" res'
  lift $ cap.log "digraph mygraph { a1 -> a2; a2 -> a3; }"
  pure unit
