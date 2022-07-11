module CirclesPink.GenerateTSD.Modules where

import Prelude

import CirclesPink.GenerateTSD.SampleModule as CirclesPink.GenerateTSD.SampleModule
import Data.ABC (A, B, Z)
import Data.Map (Map)
import Data.Map as M
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Debug.Extra (todo)
import Language.TypeScript.DTS as DTS
import PursTs.Class (toTsDef, toTsType)
import Pursts (cleanModule)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

moduleMap :: Map String (String /\ String)
moduleMap = M.fromFoldable
  [ "CirclesPink.GenerateTSD.SampleModule" /\ "./CirclesPink.GenerateTSD.SampleModule" /\ "CirclesPink_GenerateTSD_SampleModule"
  ]

modules :: Array (String /\ DTS.Module)
modules = do
  [ "CirclesPink.GenerateTSD.SampleModule" /\ cleanModule "CirclesPink_GenerateTSD_SampleModule"
      ( DTS.Module
          [ DTS.DeclValueDef (DTS.Name "gravity") (toTsType CirclesPink.GenerateTSD.SampleModule.gravity)
          , DTS.DeclValueDef (DTS.Name "myName") (toTsType CirclesPink.GenerateTSD.SampleModule.myName)
          , DTS.DeclValueDef (DTS.Name "isOff") (toTsType CirclesPink.GenerateTSD.SampleModule.isOff)
          , DTS.DeclValueDef (DTS.Name "someItems") (toTsType CirclesPink.GenerateTSD.SampleModule.someItems)
          , DTS.DeclValueDef (DTS.Name "user") (toTsType CirclesPink.GenerateTSD.SampleModule.user)
          , DTS.DeclValueDef (DTS.Name "add") (toTsType CirclesPink.GenerateTSD.SampleModule.add)
          , DTS.DeclTypeDef (DTS.Name "Baz") [] (toTsDef (Proxy :: _ CirclesPink.GenerateTSD.SampleModule.Baz))
          , DTS.DeclValueDef (DTS.Name "Foo") (toTsType CirclesPink.GenerateTSD.SampleModule.Foo)
          , DTS.DeclValueDef (DTS.Name "Bar") (toTsType CirclesPink.GenerateTSD.SampleModule.Bar)
          , DTS.DeclValueDef (DTS.Name "fromBaz") (toTsType CirclesPink.GenerateTSD.SampleModule.fromBaz)
          , DTS.DeclTypeDef (DTS.Name "Vielleicht") [] (toTsDef $ mono (Proxy :: forall a. _ (CirclesPink.GenerateTSD.SampleModule.Vielleicht a)))
          , DTS.DeclValueDef (DTS.Name "caseVielleicht") (toTsType (CirclesPink.GenerateTSD.SampleModule.caseVielleicht :: A -> B -> _))
          , DTS.DeclValueDef (DTS.Name "myNum") (toTsType (CirclesPink.GenerateTSD.SampleModule.myNum))
          , DTS.DeclValueDef (DTS.Name "myNumVar") (toTsType (CirclesPink.GenerateTSD.SampleModule.myNumVar))

          ]
      )
  ]

-- x = toMono CirclesPink.GenerateTSD.SampleModule.caseVielleicht

-- class ToMono a b where
--   toMono :: a -> b

-- instance toMono1 :: ToMono (a1 -> z -> z -> f4 a1 -> z) (A -> z -> f2 -> f4 A -> f5) where
--   toMono = unsafeCoerce 

-- instance toTsMonoProxy :: (ToMono a b) => ToMono (Proxy a) (Proxy b) where
--   toMono _ = Proxy

mono :: forall (f :: Type -> Type) a. (Proxy (f a)) -> Proxy (f A)
mono = unsafeCoerce