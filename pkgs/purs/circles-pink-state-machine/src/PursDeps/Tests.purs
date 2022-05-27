module PursDeps.Tests where

import Prelude
import Data.Argonaut (encodeJson, stringify)
import Data.Bifunctor (lmap)
import Data.Map as M
import Data.Tuple.Nested ((/\))
import PursDeps as PD
import Test.Unit as T
import Test.Unit.Assert as A

tests :: T.TestSuite
tests =
  T.suite "PursDeps" do
    T.test "parse" do
      let
        data_ :: String
        data_ =
          (stringify <<< encodeJson)
            { "Apple.Nut": { path: "", depends: [ "House.Tree" ] }
            , "House.Tree": { path: "", depends: [] :: _ String }
            }

        parsed :: PD.PursDeps
        parsed =
          M.fromFoldable
            [ [ "House", "Tree" ]
                /\ { path: "", depends: [] }
            , [ "Apple", "Nut" ]
                /\ { path: "", depends: [ [ "House", "Tree" ] ] }
            ]
      A.equal (pure parsed) (PD.parse data_ # lmap PD.printError)
