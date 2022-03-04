module PursDeps
  ( main
  ) where

import Prelude
import Control.Monad.Except (ExceptT, lift, runExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Map (Map)
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Process (exit)
import Options.Applicative (Parser, (<**>))
import Options.Applicative as O
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
  cap = { readFile: \_ -> pure "abc" }

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
data Err
  = Err

printError :: Err -> String
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
type Result r m a
  = ReaderT r (ExceptT Err m) a

runResult :: forall r m a. r -> Result r m a -> m (Either Err a)
runResult cap r = runExceptT $ runReaderT r cap

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
type Cap m
  = { readFile :: String -> ExceptT Err m String
    }

main' :: forall m. Monad m => Result { opts :: Opts, cap :: Cap m } m Unit
main' = do
  { cap, opts } <- ask
  res <- lift $ cap.readFile opts.depsJsonPath
  let
    x = spy "res" res
  pure unit
