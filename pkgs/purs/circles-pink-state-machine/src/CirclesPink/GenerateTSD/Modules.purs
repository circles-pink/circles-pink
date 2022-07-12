module CirclesPink.GenerateTSD.Modules where

import Prelude

import CirclesPink.GenerateTSD.SampleModule as CirclesPink.GenerateTSD.SampleModule
import Data.ABC (A, B)
import Data.Map (Map)
import Data.Map as M
import Data.Tuple.Nested (type (/\), (/\))
import Language.TypeScript.DTS as DTS
import PursTs (typ, val)
import Simple.Data.Maybe as Simple.Data.Maybe
import Type.Proxy (Proxy(..))

moduleMap :: Map String (String /\ String)
moduleMap = M.fromFoldable []

modules :: Array (String /\ Array (DTS.Declaration Unit))
modules = do
  [ "CirclesPink.GenerateTSD.SampleModule" /\
      [ typ (Proxy :: _ (CirclesPink.GenerateTSD.SampleModule.Baz)) "Baz"
      , typ (Proxy :: _ (CirclesPink.GenerateTSD.SampleModule.Vielleicht A)) "Vielleicht"
      , val (CirclesPink.GenerateTSD.SampleModule.Foo) "Foo"
      , val (CirclesPink.GenerateTSD.SampleModule.Bar) "Bar"
      , val (CirclesPink.GenerateTSD.SampleModule.gravity) "gravity"
      , val (CirclesPink.GenerateTSD.SampleModule.myName) "myName"
      , val (CirclesPink.GenerateTSD.SampleModule.isOff) "isOff"
      , val (CirclesPink.GenerateTSD.SampleModule.someItems) "someItems"
      , val (CirclesPink.GenerateTSD.SampleModule.user) "user"
      , val (CirclesPink.GenerateTSD.SampleModule.add) "add"
      , val (CirclesPink.GenerateTSD.SampleModule.fromBaz) "fromBaz"
      , val (CirclesPink.GenerateTSD.SampleModule.myNum) "myNum"
      , val (CirclesPink.GenerateTSD.SampleModule.myNumVar) "myNumVar"
      , val (CirclesPink.GenerateTSD.SampleModule.someMaybe) "someMaybe"

      ]
  , "Simple.Data.Maybe" /\
      [ val (Simple.Data.Maybe.Just :: A -> _) "Just"
      , val (Simple.Data.Maybe.Nothing :: _ A) "Nothing"
      , val (Simple.Data.Maybe.maybe :: _ -> (A -> B) -> _) "maybe"
      , val (Simple.Data.Maybe.maybe' :: _ -> (A -> B) -> _) "maybe'"
      ]
  , "Data.Maybe" /\
      [ typ (Proxy :: _ (Simple.Data.Maybe.Maybe A)) "Maybe"
      ]
  ]

