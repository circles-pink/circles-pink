module CirclesPink.GenerateTSD where

import CirclesPink.Prelude

import CirclesPink.GenerateTSD.Modules (moduleMap, modules)
import Data.Array as A
import Effect.Class.Console (log)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, long, metavar, strOption, (<**>))
import PursTsGen (defineModules)
import PursTsGen.Lang.TypeScript (printModule)
import Sunde as Sun

type GenerateTSOpts =
  { outputDir :: String
  }

parserOpts :: Parser GenerateTSOpts
parserOpts = ado
  outputDir <- strOption $ fold
    [ long "output-dir"
    , metavar "OUTPUT_DIR"
    , help "Dictionary containing compiled PureScript files"
    ]

  in { outputDir }

parserInfo :: ParserInfo GenerateTSOpts
parserInfo = info (parserOpts <**> helper)
  ( fold
      [ fullDesc
      , header "generate-tsd - Generate tsd files from PureScript modules"
      ]
  )

app :: Aff Unit
app = do
  opts <- liftEffect $ execParser parserInfo
  modules
    # A.sort
    # defineModules moduleMap
    # traverse_
        ( \(modName /\ mod) -> do
            let filePath = opts.outputDir <> "/" <> modName <> "/index.d.ts"
            log filePath
            writeTextFile UTF8 filePath (printModule mod)
        )
  void $ spawn "prettier" [ "--write", opts.outputDir <> "/*/index.d.ts" ]

spawn :: String -> Array String -> Aff { stderr :: String, stdout :: String }
spawn cmd args = do
  { exit, stderr, stdout } <- Sun.spawn { cmd, args, stdin: Nothing } defaultSpawnOptions
  case exit of
    Normally 0 -> pure { stderr, stdout }
    _ -> throwError $ error ("Command " <> cmd <> " failed")

main :: Effect Unit
main = launchAff_ app

