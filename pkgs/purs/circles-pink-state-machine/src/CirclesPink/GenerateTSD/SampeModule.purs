module CirclesPink.GenerateTSD.SampleModule where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import PursTsGen.Lang.TypeScript.DSL as TS
import PursTsGen.Class.ToTsDef (class ToTsDef)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

gravity :: Number
gravity = 9.81

myName :: String
myName = "Michael"

isOff :: Boolean
isOff = true

someItems :: Array Number
someItems = [ 1.0, 2.5, 3.9 ]

user :: { name :: String, online :: Boolean }
user = { name: "Me", online: true }

add :: Number -> Number -> Number
add x y = x + y

data Baz = Foo Number | Bar

-- instance toTsTypeBaz :: ToTsType Baz where
--   toTsType _ = TS.TypeConstructor (TS.QualName (Just "CirclesPink_GenerateTSD_SampleModule") "Baz") []

-- instance toTsDefBaz :: ToTsDef Baz where
--   toTsDef _ = TS.TypeOpaque (TS.QualName (Just "CirclesPink_GenerateTSD_SampleModule") "Baz") []

fromBaz :: Baz -> Number
fromBaz = case _ of
  Foo i -> i
  Bar -> 0.0

data Vielleicht a = Nur a | Nichts

-- instance toTsDefVielleicht :: ToTsDef (Vielleicht a) where
--   toTsDef _ = TS.TypeOpaque (TS.QualName (Just "CirclesPink_GenerateTSD_SampleModule") "Vielleicht") [ TS.Name "A" ]

-- instance toTsTypeVielleicht :: ToTsType a => ToTsType (Vielleicht a) where
--   toTsType _ = TS.TypeConstructor
--     (TS.QualName (Just "CirclesPink_GenerateTSD_SampleModule") "Vielleicht")
--     [ toTsType (Proxy :: _ a) ]

caseVielleicht :: forall a z. (a -> z) -> z -> Vielleicht a -> z
caseVielleicht _ z _ = z 

someMaybe :: Maybe Number
someMaybe = Just 1.2

--------------------------------------------------------------------------------

type Nums = Variant (one :: Number, two :: Number, three :: Number)

myNum :: Nums
myNum = inj (Proxy :: _ "one") 1.0

--------------------------------------------------------------------------------

type R1 r = (one :: Number | r)

type R2 r = (two :: Number | r)

type R3 r = (three :: Number | r)

type NumsVar = Variant (R1 + R2 + R3 + ())

myNumVar :: Nums
myNumVar = inj (Proxy :: _ "one") 1.0