module CirclesPink.GenerateTSD.Modules where


import CirclesPink.GenerateTSD.SampleModule as CirclesPink.GenerateTSD.SampleModule
import Data.ABC (A, B)
import Data.Map (Map)
import Data.Map as M
import Data.Nullable as Data.Nullable
import Data.Tuple.Nested (type (/\), (/\))
import Language.TypeScript.DTS as DTS
import PursTs (pursModule, typ, val)
import Simple.Data.Maybe as Simple.Data.Maybe
import Simple.Data.Number as Simple.Data.Number
import Type.Proxy (Proxy(..))

moduleMap :: Map String (String /\ String)
moduleMap = M.fromFoldable [ pursModule "Data.Maybe" ]

modules :: Array (String /\ Array DTS.Declaration)
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
      , val (Simple.Data.Maybe.bind :: _ A -> (_ -> _ B) -> _) "bind"
      , val (Simple.Data.Maybe.bindFlipped :: (_ -> _ B) -> _ A -> _) "bindFlipped"
      , val (Simple.Data.Maybe.map :: (A -> B) -> _) "map"
      , val (Simple.Data.Maybe.pure :: A -> _) "pure"
      , val (Simple.Data.Maybe.eq :: _ -> _ A -> _) "eq"
      ]
  , "Data.Maybe" /\
      [ typ (Proxy :: _ (Simple.Data.Maybe.Maybe A)) "Maybe"
      ]
  , "Data.Nullable" /\
      [ val (Data.Nullable.toNullable :: _ A -> _) "toNullable"
      ]
  , "Simple.Data.Number" /\
      [ val (Simple.Data.Number.eq) "eq"
      ]
  ]

