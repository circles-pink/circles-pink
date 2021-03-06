module CirclesPink.Garden.GenGraph where

import Prelude
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), maybe)
import Language.Dot as D
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Process (argv)
import Stadium.Graph as G
import Stadium.Reflect as R

main :: Effect Unit
main = do
  args <- argv
  let
    filePath = maybe "circles-pink-graph.dot" identity $ args !! 1
  R.reflectStateMachine _circlesStateMachine
    # G.fromStateMachineData "Circles StateMachine"
    # G.graphToDot { entryPoint: Just "landing" }
    # D.printGraph
    # writeTextFile UTF8 filePath
