module Language.Dot.Attr
  ( Attr(..)
  , C(..)
  , E(..)
  , G(..)
  , N(..)
  , S(..)
  , Shape(..)
  , box_
  , class Label
  , label
  , labelImpl
  , shape
  ) where

import Prelude
import Data.Variant (Variant, inj)
import Language.Dot.Id (Id(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Attr :: forall k. k -> Type
newtype Attr a
  = Attr { key :: Id, value :: Id }

data N

data E

data G

data S

data C

class Label :: forall k. k -> Constraint
class Label a where
  label :: String -> Attr a

instance labelN :: Label N where
  label = labelImpl

instance labelC :: Label C where
  label = labelImpl

labelImpl :: forall a. String -> Attr a
labelImpl value = Attr { key: Id "label", value: Id value }

shape :: Shape -> Attr N
shape s = Attr { key: Id "shape", value: Id $ shapeToString s }

--------------------------------------------------------------------------------
-- AttrTypes
--------------------------------------------------------------------------------
type Shape
  = Variant
      ( box :: Unit
      , ellipse :: Unit
      )

box_ :: forall v. Variant ( box :: Unit | v )
box_ = inj (Proxy :: _ "box") unit

shapeToString :: Shape -> String
shapeToString s = unsafeCoerce (unsafeCoerce s).type
