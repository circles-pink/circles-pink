module PursDeps
  ( DepEntry
  , ErrParse
  , ModuleName
  , PursDeps
  , main
  , parse
  , printError
  ) where

import Prelude
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Argonaut (Json, JsonDecodeError(..), parseJson, printJsonDecodeError)
import Data.Argonaut as AG
import Data.Array (filter, foldr)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as S
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, case_, inj, on)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Class.Console as C
import Foreign.Object (toUnfoldable) as O
import Foreign.Object as FO
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (exit)
import Options.Applicative (Parser, ParserInfo, (<**>))
import Options.Applicative (execParser, fullDesc, helper, info, long, strOption) as O
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- Opts
--------------------------------------------------------------------------------
type Opts
  = { depsJsonPath :: String }

parseOpts :: Parser Opts
parseOpts = ado
  depsJsonPath <-
    O.strOption
      $ fold [ O.long "depsJsonPath" ]
  in { depsJsonPath }

prog :: ParserInfo Opts
prog =
  O.info (parseOpts <**> O.helper)
    $ fold [ O.fullDesc ]

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
  = Map ModuleName DepEntry

type DepEntry
  = { path :: String, depends :: Array ModuleName }

type ModuleName
  = Array String

--------------------------------------------------------------------------------
-- ModuleTree
--------------------------------------------------------------------------------
newtype ModuleForest
  = ModuleForest
  ( Map String
      { exists :: Boolean
      , depends :: Array ModuleName
      , subModules :: ModuleForest
      }
  )

emptyModuleForest :: ModuleForest
emptyModuleForest = ModuleForest M.empty

insert :: ModuleName /\ DepEntry -> ModuleForest -> ModuleForest
insert (mn /\ de) (ModuleForest mf) = case A.uncons mn of
  Nothing -> ModuleForest mf
  Just { head, tail }
    | tail == [] ->
      ModuleForest
        $ M.insertWith
            (\old _ -> old { exists = true })
            head
            { exists: true, subModules: emptyModuleForest, depends: de.depends }
            mf
  Just { head, tail } ->
    ModuleForest
      $ M.insertWith
          (\old _ -> old { subModules = insert (tail /\ de) old.subModules })
          head
          { exists: false, subModules: insert (tail /\ de) emptyModuleForest, depends: de.depends }
          mf

--------------------------------------------------------------------------------
-- Result
--------------------------------------------------------------------------------
type Result' r env m a
  = ReaderT env (ExceptV (Err + r) m) a

type Env m
  = { opts :: Opts, cap :: Cap m }

type Result r m a
  = Result' r (Env m) m a

runResult :: forall env m a. env -> Result' () env m a -> m (Either (Variant (Err ())) a)
runResult cap' r = runExceptT $ runReaderT r cap'

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------
decodePursDeps :: Json -> Either JsonDecodeError PursDeps
decodePursDeps j = do
  obj <- AG.toObject j # note (TypeMismatch "object")
  let
    xs :: Array (String /\ _)
    xs = O.toUnfoldable obj
  xs' <- traverse (decodeKV (decodeModuleName <<< AG.fromString) decodeDepEntry) xs
  pure $ M.fromFoldable xs'

decodeModuleName :: Json -> Either JsonDecodeError ModuleName
decodeModuleName j = do
  s <- AG.toString j # note (TypeMismatch "string")
  pure $ S.split (Pattern ".") s

decodeDepEntry :: Json -> Either JsonDecodeError DepEntry
decodeDepEntry j = do
  obj <- AG.toObject j # note (TypeMismatch "object")
  path <-
    FO.lookup "path" obj
      # note (AtKey "path" $ MissingValue)
      >>= (AG.toString >>> note (TypeMismatch "string"))
  depends' <-
    FO.lookup "depends" obj
      # note (AtKey "depends" $ MissingValue)
      >>= (AG.toArray >>> note (TypeMismatch "string"))
  depends <- traverse decodeModuleName depends'
  pure $ { path, depends }

decodeKV ::
  forall k v.
  (String -> Either JsonDecodeError k) ->
  (Json -> Either JsonDecodeError v) ->
  String /\ Json -> Either JsonDecodeError (k /\ v)
decodeKV decK decV (k /\ v) = do
  k' <- decK k
  v' <- decV v
  pure (k' /\ v')

parse :: forall r. String -> Either (Variant (ErrParse + r)) PursDeps
parse s = parseJson s >>= decodePursDeps # lmap (inj (Proxy :: _ "errParse"))

--------------------------------------------------------------------------------
-- Dot
--------------------------------------------------------------------------------
data Dot
  = Dot

forestToDot :: ModuleForest -> Dot
forestToDot _ = Dot

printDot :: Dot -> String
printDot _ = "digraph mygraph { a1 -> a2; a2 -> a3; }"

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
filterPursDeps :: Regex -> Map ModuleName DepEntry -> Map ModuleName DepEntry
filterPursDeps reg mp =
  let
    mp' = M.filter (\v -> not $ R.test reg v.path) mp
  in
    map (\x -> x { depends = filter (\y -> M.member y mp') x.depends }) mp'

--------------------------------------------------------------------------------
-- Main'
--------------------------------------------------------------------------------
type Cap m
  = { readFile :: CapReadFile m
    , log :: CapLog m
    }

type CapReadFile m
  = forall r. String -> ExceptV (ErrReadFile + r) m String

type CapLog m
  = forall r. String -> ExceptV r m Unit

readFile :: forall r m. Monad m => Result r m String
readFile = do
  cap' :: Cap m <- asks _.cap
  opts <- asks _.opts
  lift $ cap'.readFile opts.depsJsonPath

parseDeps :: forall r m. Monad m => String -> Result r m PursDeps
parseDeps = lift <<< except <<< parse

filterDeps :: PursDeps -> PursDeps
filterDeps = filterPursDeps (unsafeRegex "\\.spago" noFlags)

writeForest :: PursDeps -> ModuleForest
writeForest = foldr insert emptyModuleForest <<< M.toUnfoldable

logStr :: forall r m. Monad m => String -> Result r m Unit
logStr x = do
  cap' :: Cap m <- asks _.cap
  lift $ cap'.log x

main' :: forall r m. Monad m => Result r m Unit
main' = do
  modForest <-
    readFile
      >>= parseDeps
      <#> (filterDeps >>> writeForest)
  forestToDot modForest
    # printDot
    # logStr

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
main :: Effect Unit
main = do
  opts <- O.execParser prog
  r <- runResult { opts, cap } main'
  case r of
    Left e -> do
      error $ printError e
      exit 1
    Right _ -> exit 0

cap :: Cap Effect
cap =
  { readFile: capReadFile
  , log: capLog
  }

capReadFile :: CapReadFile Effect
capReadFile p =
  readTextFile UTF8 p
    # try
    <#> lmap (const $ inj (Proxy :: _ "errReadFile") p)
    # ExceptT

capLog :: CapLog Effect
capLog s = liftEffect $ C.log s
