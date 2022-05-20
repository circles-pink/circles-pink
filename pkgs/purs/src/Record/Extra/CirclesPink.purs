module Record.Extra.CirclesPink where

import Prelude
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy)

instance zipProps ::
  ( IsSymbol sym
  , Row.Cons sym (a -> b) x fns
  ) =>
  MappingWithIndex (ZipProps fns) (Proxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns

newtype ZipProps fns = ZipProps { | fns }

zipRecord :: forall r1 r2 r. HMapWithIndex (ZipProps r1) (Record r2) (Record r) => Record r1 -> Record r2 -> Record r
zipRecord = hmapWithIndex <<< ZipProps
