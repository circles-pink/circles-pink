module CirclesPink.GenGraph where

import Prelude
import CirclesPink.StateMachine (_circlesStateMachine)
import Data.Array ((!!))
import Data.Maybe (maybe)
import Dot as D
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
    # G.graphToDot
    # D.toString
    # writeTextFile UTF8 filePath
