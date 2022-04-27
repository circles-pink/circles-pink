module Reference.Operators where

import Prelude
import Control.Semigroupoid (composeFlipped)
import Data.Functor (mapFlipped)
import Effect (Effect)
import Undefined (undefined)

type A
  = String

type B
  = Int

type C
  = Boolean

type D
  = Char

fnAtoB :: A -> B
fnAtoB = undefined

fnBtoC :: B -> C
fnBtoC = undefined

fnCtoD :: C -> D
fnCtoD = undefined

valA :: A
valA = undefined

valB :: B
valB = undefined

valC :: D
valC = undefined

valD :: D
valD = undefined

type Box a
  = Effect a

boxOfA :: Box A
boxOfA = undefined

boxOfB :: Box B
boxOfB = undefined

boxOfC :: Box C
boxOfC = undefined

boxOfD :: Box D
boxOfD = undefined

--------------------------------------------------------------------------------
-- compose (<<<)
--------------------------------------------------------------------------------
sample1_compose :: A -> C
sample1_compose = fnBtoC <<< fnAtoB

sample2_compose :: A -> D
sample2_compose = fnCtoD <<< fnBtoC <<< fnAtoB

sample3_compose :: A -> D
sample3_compose = (fnCtoD <<< fnBtoC) <<< fnAtoB

sample4_compose :: A -> D
sample4_compose = fnCtoD <<< (fnBtoC <<< fnAtoB)

--------------------------------------------------------------------------------
-- compose (<<<) / _prefix
--------------------------------------------------------------------------------
sample1_compose_prefix :: A -> C
sample1_compose_prefix = compose fnBtoC fnAtoB

--------------------------------------------------------------------------------
-- composeFlipped (>>>)
--------------------------------------------------------------------------------
sample1_composeFlipped :: A -> C
sample1_composeFlipped = fnAtoB >>> fnBtoC

sample2_composeFlipped :: A -> D
sample2_composeFlipped = fnAtoB >>> fnBtoC >>> fnCtoD

--------------------------------------------------------------------------------
-- composeFlipped (>>>) / _prefix
--------------------------------------------------------------------------------
sample1_composeFlipped_prefix :: A -> C
sample1_composeFlipped_prefix = composeFlipped fnAtoB fnBtoC

--------------------------------------------------------------------------------
-- apply ($)
--------------------------------------------------------------------------------
sample1_apply :: B
sample1_apply = fnAtoB $ valA

sample2_apply :: C
sample2_apply = fnBtoC $ fnAtoB $ valA

sample3_apply :: C
sample3_apply = fnBtoC $ (fnAtoB $ valA)

--------------------------------------------------------------------------------
-- applyFlipped (#)
--------------------------------------------------------------------------------
sample1_applyFlipped :: B
sample1_applyFlipped = valA # fnAtoB

sample2_applyFlipped :: C
sample2_applyFlipped = valA # fnAtoB # fnBtoC

sample3_applyFlipped :: C
sample3_applyFlipped = (valA # fnAtoB) # fnBtoC

--------------------------------------------------------------------------------
-- map (<$>)
--------------------------------------------------------------------------------
sample1_map :: Box B
sample1_map = fnAtoB <$> boxOfA

sample2_map :: Box C
sample2_map = fnBtoC <$> fnAtoB <$> boxOfA

sample3_map :: Box C
sample3_map = fnBtoC <$> (fnAtoB <$> boxOfA)

--------------------------------------------------------------------------------
-- map (<$>) / _prefix
--------------------------------------------------------------------------------
sample1_map_prefix :: Box B
sample1_map_prefix = map fnAtoB boxOfA

sample3_map_prefix :: Box C
sample3_map_prefix = map fnBtoC (map fnAtoB boxOfA)

--------------------------------------------------------------------------------
-- mapFlipped (<$>)
--------------------------------------------------------------------------------
sample1_mapFlipped :: Box B
sample1_mapFlipped = boxOfA <#> fnAtoB

sample2_mapFlipped :: Box C
sample2_mapFlipped = boxOfA <#> fnAtoB <#> fnBtoC

sample3_mapFlipped :: Box C
sample3_mapFlipped = (boxOfA <#> fnAtoB) <#> fnBtoC

--------------------------------------------------------------------------------
-- mapFlipped (<$>) / _prefix
--------------------------------------------------------------------------------
sample1_mapFlipped_prefix :: Box B
sample1_mapFlipped_prefix = mapFlipped boxOfA fnAtoB

sample3_mapFlipped_prefix :: Box C
sample3_mapFlipped_prefix = mapFlipped (mapFlipped boxOfA fnAtoB) fnBtoC
