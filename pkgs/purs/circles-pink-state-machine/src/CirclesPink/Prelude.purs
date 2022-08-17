module CirclesPink.Prelude (module Exp) where


import Control.Monad.Except (class MonadError, class MonadTrans, Except, ExceptT(..), catchJust, except, lift, mapExcept, mapExceptT, runExcept, runExceptT, withExcept, withExceptT) as Exp
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exp
import Effect.Aff (Aff, BracketConditions, Canceler(..), Error, Fiber, Milliseconds(..), ParAff, apathize, attempt, bracket, cancelWith, catchError, delay, effectCanceler, error, fiberCanceler, finally, forkAff, generalBracket, invincible, joinFiber, killFiber, launchAff, launchAff_, launchSuspendedAff, makeAff, message, never, nonCanceler, parallel, runAff, runAff_, runSuspendedAff, sequential, supervise, suspendAff, throwError, try) as Exp
import Effect.Aff.Class (class MonadAff, liftAff) as Exp
import Data.Variant (class Contractable, class VariantBounded, class VariantBoundedEnums, class VariantEqs, class VariantMatchCases, class VariantOrds, class VariantShows, Unvariant(..), Unvariant', Variant, case_, contract, default, expand, inj, match, on, onMatch, prj, revariant, unvariant, variantBounded, variantBoundedEnums, variantEqs, variantOrds, variantShows) as Exp
import Data.Either (Either(..), choose, either, fromLeft, fromLeft', fromRight, fromRight', hush, isLeft, isRight, note, note') as Exp
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Exp
import Data.Argonaut (class DecodeJson, class EncodeJson, JCursor(..), Json, JsonDecodeError(..), JsonPrim(..), _Array, _Boolean, _JsonArray, _JsonBoolean, _JsonNull, _JsonNumber, _JsonObject, _JsonString, _Null, _Number, _Object, _String, assoc, assocOptional, caseJson, caseJsonArray, caseJsonBoolean, caseJsonNull, caseJsonNumber, caseJsonObject, caseJsonString, cursorGet, cursorSet, decodeJson, defaultField, downField, downIndex, encodeJson, extend, extendOptional, fromArray, fromBoolean, fromNumber, fromObject, fromPrims, fromString, getField, getFieldOptional, getFieldOptional', inferEmpty, insideOut, isArray, isBoolean, isNull, isNumber, isObject, isString, jsonEmptyArray, jsonEmptyObject, jsonEmptyString, jsonFalse, jsonNull, jsonParser, jsonSingletonArray, jsonSingletonObject, jsonTrue, jsonZero, parseJson, primBool, primNull, primNum, primStr, primToJson, print, printJsonDecodeError, runJsonPrim, stringify, stringifyWithIndent, toArray, toBoolean, toNull, toNumber, toObject, toPrims, toString, (.!=), (.:), (.:!), (.:?), (:=), (:=?), (~>), (~>?)) as Exp
import Debug (class DebugWarning, debugger, spy, spyWith, trace, traceM) as Exp
import Debug.Extra (todo) as Exp
import Data.Newtype (class Newtype, ala, alaF, collect, over, over2, overF, overF2, un, under, under2, underF, underF2, unwrap, wrap) as Exp
import Partial.Unsafe (unsafeCrashWith, unsafePartial) as Exp
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry) as Exp
import Data.Tuple.Nested (type (/\), T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, Tuple1, Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9, curry1, curry10, curry2, curry3, curry4, curry5, curry6, curry7, curry8, curry9, get1, get10, get2, get3, get4, get5, get6, get7, get8, get9, over1, over10, over3, over4, over5, over6, over7, over8, over9, tuple1, tuple10, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9, uncurry1, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, uncurry6, uncurry7, uncurry8, uncurry9, (/\)) as Exp
import Type.Proxy (Proxy(..), Proxy2(..), Proxy3(..)) as Exp
import Type.Row (class Cons, class Lacks, class Nub, class Union, type (+), RProxy(..), RowApply) as Exp
import Control.Monad.Except.Checked (ExceptV, handleError, handleErrors, safe) as Exp
import Control.Monad.State.Class (class MonadState, get, gets, modify, modify_, put, state) as Exp
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Exp
import Effect.Class (class MonadEffect, liftEffect) as Exp
import Effect (Effect, forE, foreachE, untilE, whileE) as Exp
import Data.Traversable (class Foldable, class Traversable, Accum, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for, for_, intercalate, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, scanl, scanr, sequence, sequenceDefault, sequence_, sum, traverse, traverseDefault, traverse_) as Exp
import Data.Newtype.Extra (applyUnwrapped, applyUnwrappedFlipped, ($-), (-#)) as Exp
import Test.Spec (Spec, describe, it) as Exp
import Test.Spec.Assertions (expectError, fail, shouldContain, shouldEqual, shouldNotContain, shouldNotEqual) as Exp
import Partial (crashWith) as Exp
import Test.Spec.Extra (describeFn) as Exp
import Data.Foldable (findMap, foldM, indexl, indexr, length, lookup, null, oneOfMap, product, surround, surroundMap) as Exp
import PursTsGen (class ToPursType, class GenRecord, class GenToTsDefProd, class GenToTsDefSum, class GenVariant, class ToTsDef, class ToTsType, PursType(..), classDef, cleanModule, constructor, defPredicateFn, defaultToPursType, defaultToTsDef, defaultToTsType, defineModules, genRecord, genToTsDefProd, genToTsDefSum, genToTsDefSum', genVariant, genericToTsDef, instanceDef, pursModule, toTsDef, toTsType, typeAlias, typeDef, value) as Exp