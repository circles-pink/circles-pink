module PursTsGen.Lang.TypeScript.Traversal
  ( defaultVisitor
  , defaultVisitorM
  , rewriteModuleTopDown
  , rewriteModuleTopDownM
  , rewriteTypeTopDown
  , rewriteTypeTopDownM
  ) where

import PursTsGen.Prelude
import Prim hiding (Row, Type)
import PursTsGen.Lang.TypeScript.Types (Declaration(..), Module(..), ModuleBody(..), Type(..))
import Prim as P

type Rewrite f a = a -> f a

type PureRewrite a = a -> a

type OnType :: forall k. (P.Type -> k) -> P.Row k -> P.Row k
type OnType t r = (onType :: t Type | r)

type OnDeclaration :: forall k. (P.Type -> k) -> P.Row k -> P.Row k
type OnDeclaration t r = (onDeclaration :: t Declaration | r)

type OnTs :: forall k. (P.Type -> k) -> P.Row k
type OnTs t = OnType t + OnDeclaration t + ()

defaultVisitorM :: forall f. Applicative f => { | OnTs (Rewrite f) }
defaultVisitorM =
  { onType: pure
  , onDeclaration: pure
  }

defaultVisitor :: { | OnTs PureRewrite }
defaultVisitor =
  { onType: identity
  , onDeclaration: identity
  }

traverseType :: forall f r. Applicative f => { | OnType (Rewrite f) + r } -> Rewrite f Type
traverseType { onType } =
  case _ of
    TypeArray x -> TypeArray <$> onType x
    TypeRecord xs -> TypeRecord <$> traverse (traverse onType) xs
    TypeFunction q args ret -> TypeFunction q <$> traverse (traverse onType) args <*> onType ret
    TypeConstructor n xs -> TypeConstructor n <$> traverse onType xs
    TypeUnion x y -> TypeUnion <$> onType x <*> onType y
    t -> pure t

traverseDeclaration :: forall f r. Applicative f => { | OnType (Rewrite f) + r } -> Rewrite f Declaration
traverseDeclaration { onType } =
  case _ of
    DeclTypeDef n ns t -> DeclTypeDef n ns <$> onType t
    DeclValueDef n t -> DeclValueDef n <$> onType t
    t -> pure t

traverseModuleBody :: forall f r. Applicative f => { | OnType (Rewrite f) + r } -> Rewrite f ModuleBody
traverseModuleBody v (ModuleBody xs) = ModuleBody <$> traverse (traverseDeclaration v) xs

traverseModule :: forall f r. Applicative f => { | OnType (Rewrite f) + r } -> Rewrite f Module
traverseModule v (Module mh mb) = Module mh <$> traverseModuleBody v mb

topDownTraversal :: forall m. Monad m => { | OnTs (Rewrite m) } -> { | OnTs (Rewrite m) }
topDownTraversal visitor = visitor'
  where
  visitor' =
    { onType: \a -> visitor.onType a >>= traverseType visitor'
    , onDeclaration: \a -> visitor.onDeclaration a >>= traverseDeclaration visitor'
    }

topDownPureTraversal :: { | OnTs PureRewrite } -> { | OnTs (Rewrite (Free Identity)) }
topDownPureTraversal visitor = visitor'
  where
  visitor' =
    { onType: \a -> pure (visitor.onType a) >>= traverseType visitor'
    , onDeclaration: \a -> pure (visitor.onDeclaration a) >>= traverseDeclaration visitor'
    }

rewriteTopDown
  :: forall g
   . ({ | OnTs (Rewrite (Free Identity)) } -> Rewrite (Free Identity) g)
  -> { | OnTs PureRewrite }
  -> PureRewrite g
rewriteTopDown traversal visitor = do
  let visitor' = topDownPureTraversal visitor
  runFree (un Identity) <<< traversal visitor'

rewriteTopDownM :: forall m g. Monad m => ({ | OnTs (Rewrite m) } -> Rewrite m g) -> { | OnTs (Rewrite m) } -> Rewrite m g
rewriteTopDownM traversal visitor = do
  let visitor' = topDownTraversal visitor
  traversal visitor'

rewriteModuleTopDown :: { | OnTs PureRewrite } -> PureRewrite Module
rewriteModuleTopDown = rewriteTopDown traverseModule

rewriteModuleTopDownM :: forall m. Monad m => { | OnTs (Rewrite m) } -> Rewrite m Module
rewriteModuleTopDownM = rewriteTopDownM traverseModule

rewriteTypeTopDown :: { | OnTs PureRewrite } -> PureRewrite Type
rewriteTypeTopDown = rewriteTopDown traverseType

rewriteTypeTopDownM :: forall m. Monad m => { | OnTs (Rewrite m) } -> Rewrite m Type
rewriteTypeTopDownM = rewriteTopDownM traverseType

