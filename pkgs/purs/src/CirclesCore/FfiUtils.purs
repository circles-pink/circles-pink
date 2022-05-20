module CirclesCore.FfiUtils
  ( MapArg
  , MapErr
  , MapOk
  , mapFn1
  , mapFn2
  , mapFn3
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..), except)
import Control.Monad.Except.Checked (ExceptV)
import Control.Promise (Promise, toAff)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Variant (Variant)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)

type MapArg h e l = h -> Either (Variant e) l

type MapErr e = Error -> Variant e

type MapOk l e h = l -> Either (Variant e) h

mapFn3
  :: forall h1 h2 h3 l1 l2 l3 h l e
   . Fn3 l1 l2 l3 (Promise l)
  -> MapArg h1 e l1
  -> MapArg h2 e l2
  -> MapArg h3 e l3
  -> MapErr e
  -> MapOk l e h
  -> h1
  -> h2
  -> h3
  -> ExceptV e Aff h
mapFn3 f ma1 ma2 ma3 me mr h1 h2 h3 = do
  l1 <- except $ ma1 h1
  l2 <- except $ ma2 h2
  l3 <- except $ ma3 h3
  runFn3 f l1 l2 l3 # mapReturn me mr

mapFn2
  :: forall h1 h2 l1 l2 h l e
   . Fn2 l1 l2 (Promise l)
  -> MapArg h1 e l1
  -> MapArg h2 e l2
  -> MapErr e
  -> MapOk l e h
  -> h1
  -> h2
  -> ExceptV e Aff h
mapFn2 f ma1 ma2 me mr h1 h2 = do
  l1 <- except $ ma1 h1
  l2 <- except $ ma2 h2
  runFn2 f l1 l2 # mapReturn me mr

mapFn1
  :: forall h1 l1 h l e
   . Fn1 l1 (Promise l)
  -> MapArg h1 e l1
  -> MapErr e
  -> MapOk l e h
  -> h1
  -> ExceptV e Aff h
mapFn1 f ma1 me mr h1 = do
  l1 <- except $ ma1 h1
  runFn1 f l1 # mapReturn me mr

mapReturn :: forall h l e. MapErr e -> MapOk l e h -> Promise l -> ExceptV e Aff h
mapReturn me mr p = toAff p # attempt <#> lmap me # ExceptT >>= (mr >>> except)
