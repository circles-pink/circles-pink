module CirclesPink.GenerateTSD.Modules where

import Prelude

import CirclesCore as CirclesPink.Data.User
import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.PrivateKey as CirclesPink.Data.PrivateKey
import CirclesPink.Data.TrustConnection as CirclesPink.Data.TrustConnection
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.Data.TrustState as CirclesPink.Data.TrustState
import CirclesPink.Data.UserIdent as CirclesPink.Data.UserIdent
import CirclesPink.Garden.StateMachine.Action as CirclesPink.Garden.StateMachine.Action
import CirclesPink.Garden.StateMachine.Control.EnvControl as CirclesPink.Garden.StateMachine.Control.EnvControl
import CirclesPink.Garden.StateMachine.Direction as CirclesPink.Garden.StateMachine.Direction
import CirclesPink.Garden.StateMachine.State as CirclesPink.Garden.StateMachine.State
import CirclesPink.Garden.StateMachine.State.Dashboard as CirclesPink.Garden.StateMachine.State.Dashboard
import CirclesPink.GenerateTSD.Replace as R
import CirclesPink.GenerateTSD.TypeClasses (ClassOrd, ORD(..))
import Data.Argonaut as Data.Argonaut
import Data.BN as Data.BN
import Data.DateTime.Instant as Data.DateTime.Instant
import Data.Either (Either)
import Data.Either as Data.Either
import Data.Graph.Errors (ErrLookupEdge, ErrNeighborNodes, ErrNeighborhood, ErrLookupNode)
import Data.IxGraph as Data.IxGraph
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Newtype (class Newtype)
import Data.Nullable as Data.Nullable
import Data.Ord as Data.Ord
import Data.Pair as Data.Pair
import Data.Tuple (fst)
import Data.Tuple as Data.Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Network.Ethereum.Core.Signatures as Network.Ethereum.Core.Signatures
import Prelude as Data.Unit
import PursTsGen (classDef, defPredicateFn, instanceDef, pursModule, toTsType, typeDef, value)
import PursTsGen.Class.ToTsType (class ToTsType)
import PursTsGen.Data.ABC (A(..), B(..), C, E, L, N, Z)
import PursTsGen.Lang.TypeScript.DSL as TS
import RemoteData (RemoteData)
import RemoteData as RemoteData
import Simple.Data.Array as Simple.Data.Array
import Simple.Data.Maybe as Simple.Data.Maybe
import Simple.Data.Tuple as Simple.Data.Tuple
import Type.Proxy (Proxy(..))

moduleMap :: Map String (String /\ String)
moduleMap = modules <#> (fst >>> pursModule) # M.fromFoldable

_Ord :: forall a. Ord a => ToTsType a => a -> TS.Type
_Ord a = TS.TypeConstructor (TS.QualName (Just "Data_Ord") "Ord") [ toTsType a ]

_Newtype :: forall a b. Newtype a b => ToTsType a => ToTsType b => a -> b -> TS.Type
_Newtype a b = TS.TypeConstructor (TS.QualName (Just "Data_Newtype") "Newtype") [ toTsType a, toTsType b ]

data Any

instance toTsTypeAny :: ToTsType Any where
  toTsType _ = TS.any

modules :: Array (String /\ Array TS.Declaration)
modules =
  [ "CirclesPink.Garden.StateMachine.State.Dashboard" /\ join
      [ R.typeAlias "CirclesGraph"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.Dashboard.CirclesGraph)
      , R.typeAlias "DashboardState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.Dashboard.DashboardState_)
      ]

  , "Data.DateTime.Instant" /\ join
      [ R.typeDef "--"
          (Proxy :: _ Data.DateTime.Instant.Instant)
      ]

  , "CirclesPink.Garden.StateMachine.Action" /\ join
      [ R.typeAlias "CirclesAction"
          (Proxy :: _ CirclesPink.Garden.StateMachine.Action.CirclesAction)
      , R.value "_circlesAction" []
          (CirclesPink.Garden.StateMachine.Action._circlesAction)
      , R.value "_landingAction" []
          (CirclesPink.Garden.StateMachine.Action._landingAction)
      , R.value "_loginAction" []
          (CirclesPink.Garden.StateMachine.Action._loginAction)
      , R.value "_askUsernameAction" []
          (CirclesPink.Garden.StateMachine.Action._askUsernameAction)
      , R.value "_askEmailAction" []
          (CirclesPink.Garden.StateMachine.Action._askEmailAction)
      , R.value "_infoSecurityAction" []
          (CirclesPink.Garden.StateMachine.Action._infoSecurityAction)
      ]

  , "Network.Ethereum.Core.Signatures" /\ join
      [ R.typeDef "--"
          (Proxy :: _ Network.Ethereum.Core.Signatures.Address)
      ]

  , "CirclesPink.Garden.StateMachine.State" /\ join
      [ R.typeAlias "LandingState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.LandingState)
      , R.typeAlias "LoginState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.LoginState)
      , R.typeAlias "UserData"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.UserData)
      , R.typeAlias "CirclesState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.CirclesState)
      ]

  , "Data.Argonaut" /\ join
      [ R.typeDef "--"
          (Proxy :: _ (Data.Argonaut.JsonDecodeError))
      ]

  , "CirclesPink.Garden.StateMachine.Control.EnvControl" /\ join
      [ R.typeDef "--"
          (Proxy :: _ (CirclesPink.Garden.StateMachine.Control.EnvControl.StorageType))
      ]

  , "RemoteData" /\ join
      [ R.typeDef "--"
          (Proxy :: _ (RemoteData N L E A))
      , R.value "unwrap" []
          (RemoteData.unwrap :: _ N L E A -> _)
      , R.value "unRemoteData" []
          (RemoteData.unRemoteData :: _ -> _ N L E A -> Z)
      ]

  , "Data.IxGraph" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ (Data.IxGraph.IxGraph A B C))
        , R.typeDef "--"
            (Proxy :: _ (Data.IxGraph.NeighborConnectivity A))
        , R.value "neighborNodes"
            [ _Ord ORD ]
            (Data.IxGraph.neighborNodes :: ORD -> _ ORD B C -> _ (_ (ErrNeighborNodes ORD ())) _)
        , R.value "neighborhood"
            [ _Ord ORD ]
            (Data.IxGraph.neighborhood :: ORD -> _ ORD B C -> _ (_ (ErrNeighborhood ORD ())) _)
        , R.value "lookupEdge"
            [ _Ord ORD ]
            (Data.IxGraph.lookupEdge :: _ -> _ ORD B C -> _ (_ (ErrLookupEdge ORD ())) _)
        , R.value "lookupNode"
            [ _Ord ORD ]
            (Data.IxGraph.lookupNode :: _ -> _ ORD B C -> _ (_ (ErrLookupNode ORD ())) _)
        , R.value "unNeighborConnectivity" [] (Data.IxGraph.unNeighborConnectivity :: _ -> _ A -> C)
        ]

  , "CirclesPink.Data.Address" /\
      join
        [ typeDef "--"
            (Proxy :: _ CirclesPink.Data.Address.Address)
        , instanceDef "ordAddress"
            (_Ord (Proxy :: _ CirclesPink.Data.Address.Address))
        ]

  , "CirclesPink.Data.User" /\
      join
        [ R.typeAlias "User"
            (Proxy :: _ CirclesPink.Data.User.User)
        ]

  , "CirclesPink.Garden.StateMachine.Direction" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ CirclesPink.Garden.StateMachine.Direction.Direction)
        , R.value "unDirection" [] (CirclesPink.Garden.StateMachine.Direction.unDirection :: _ -> _ -> Z)
        ]

  , "CirclesPink.Data.PrivateKey" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ CirclesPink.Data.PrivateKey.PrivateKey)
        ]

  , "Simple.Data.Array" /\
      join
        [ value "mapArray" []
            (Simple.Data.Array.mapArray :: (A -> B) -> _)
        ]

  , "CirclesPink.Data.TrustNode" /\
      join
        [ typeDef "--"
            (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode)
        , value "unwrap" []
            CirclesPink.Data.TrustNode.unwrap
        , value "initTrustNode" []
            CirclesPink.Data.TrustNode.initTrustNode
        , instanceDef "ordTrustNode"
            (_Ord (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode))
        ]

  , "CirclesPink.Data.TrustState" /\
      join
        [ typeDef "--"
            (Proxy :: _ CirclesPink.Data.TrustState.TrustState)
        , value "unTrustState" []
            CirclesPink.Data.TrustState.unTrustState
        , value "isUntrusted" []
            CirclesPink.Data.TrustState.isUntrusted
        , value "isTrusted" []
            CirclesPink.Data.TrustState.isTrusted
        , value "isLoadingTrust" []
            CirclesPink.Data.TrustState.isLoadingTrust
        , value "isLoadingUntrust" []
            CirclesPink.Data.TrustState.isLoadingUntrust
        , value "isPendingTrust" []
            CirclesPink.Data.TrustState.isPendingTrust
        , value "isPendingUntrust" []
            CirclesPink.Data.TrustState.isPendingUntrust
        , value "initUntrusted" []
            CirclesPink.Data.TrustState.initUntrusted
        ]

  , "CirclesPink.Data.UserIdent" /\
      join
        [ typeDef "--"
            (Proxy :: _ CirclesPink.Data.UserIdent.UserIdent)
        -- , value "unwrap" [] CirclesPink.Data.UserIdent.unwrap
        , value "getIdentifier" []
            CirclesPink.Data.UserIdent.getIdentifier
        , value "fromUser" []
            CirclesPink.Data.UserIdent.fromUser
        , value "getAddress" []
            CirclesPink.Data.UserIdent.getAddress
        ]

  , "CirclesPink.Data.TrustConnection" /\
      join
        [ typeDef "--"
            (Proxy :: _ (CirclesPink.Data.TrustConnection.TrustConnection))
        , R.value "unTrustConnection" []
            (CirclesPink.Data.TrustConnection.unTrustConnection :: _ -> _ -> C)
        ]

  , "Data.Either" /\
      join
        [ typeDef "--"
            (Proxy :: _ (Data.Either.Either A B))
        , value "either" []
            (Data.Either.either :: _ -> _ -> _ A B -> C)
        , value "hush" []
            (Data.Either.hush :: _ A B -> _)
        , defPredicateFn "isLeft" []
            (Data.Either.isLeft :: Either A B -> Boolean)
            (TS.mkType (TS.qualName_ "Either_Left") [ toTsType A ])
        , defPredicateFn "isRight" []
            (Data.Either.isRight :: _ A B -> _)
            (TS.mkType (TS.qualName_ "Either_Right") [ toTsType B ])
        ]

  , "Data.Tuple" /\
      join
        [ typeDef "--"
            (Proxy :: _ (Data.Tuple.Tuple A B))
        ]

  , "Simple.Data.Tuple" /\
      join
        [ value "unTuple" []
            (Simple.Data.Tuple.unTuple :: _ -> _ A B -> C)
        ]

  , "Data.Pair" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ (Data.Pair.Pair A))
        ]

  , "Data.Maybe" /\
      join
        [ typeDef "--"
            (Proxy :: _ (Data.Maybe.Maybe A))
        , value "maybe" []
            (Data.Maybe.maybe :: _ -> (A -> B) -> _)
        ]

  , "Simple.Data.Maybe" /\
      join
        [ value "unMaybe" []
            (Simple.Data.Maybe.unMaybe :: _ -> _ A -> C)
        ]

  , "Data.Ord" /\
      join
        [ classDef "Ord"
            (Proxy :: _ (Data.Ord.Ord ORD => Unit))
            (Proxy :: _ (ClassOrd A))
        ]

  , "Data.Nullable" /\
      join
        [ value "toNullable" []
            (Data.Nullable.toNullable :: _ A -> _)
        ]

  , "Data.Unit" /\
      join
        [ typeDef "--"
            (Proxy :: _ Data.Unit.Unit)
        , value "unit" [] Data.Unit.unit
        ]

  , "Data.BN" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ Data.BN.BN)
        ]

  , "PursTsGen.Prim" /\
      join
        [ typeDef "--"
            (Proxy :: _ Int)
        ]

  ]

--        "CirclesPink.GenerateTSD.SampleModule" /\
--       [ typ (Proxy :: _ (CirclesPink.GenerateTSD.SampleModule.Baz)) "Baz"
--       , typ (Proxy :: _ (CirclesPink.GenerateTSD.SampleModule.Vielleicht A)) "Vielleicht"
--       , val (CirclesPink.GenerateTSD.SampleModule.Foo) "Foo"
--       , val (CirclesPink.GenerateTSD.SampleModule.Bar) "Bar"
--       , val (CirclesPink.GenerateTSD.SampleModule.gravity) "gravity"
--       , val (CirclesPink.GenerateTSD.SampleModule.myName) "myName"
--       , val (CirclesPink.GenerateTSD.SampleModule.isOff) "isOff"
--       , val (CirclesPink.GenerateTSD.SampleModule.someItems) "someItems"
--       , val (CirclesPink.GenerateTSD.SampleModule.user) "user"
--       , val (CirclesPink.GenerateTSD.SampleModule.add) "add"
--       , val (CirclesPink.GenerateTSD.SampleModule.fromBaz) "fromBaz"
--       , val (CirclesPink.GenerateTSD.SampleModule.myNum) "myNum"
--       , val (CirclesPink.GenerateTSD.SampleModule.myNumVar) "myNumVar"
--       , val (CirclesPink.GenerateTSD.SampleModule.someMaybe) "someMaybe"
--       ]
--   , "Simple.Data.Maybe" /\
--       [ val (Simple.Data.Maybe.Just :: A -> _) "Just"
--       , val (Simple.Data.Maybe.Nothing :: _ A) "Nothing"
--       , val (Simple.Data.Maybe.maybe :: _ -> (A -> B) -> _) "maybe"
--       , val (Simple.Data.Maybe.maybe' :: _ -> (A -> B) -> _) "maybe'"
--       , val (Simple.Data.Maybe.bind :: _ A -> (_ -> _ B) -> _) "bind"
--       , val (Simple.Data.Maybe.bindFlipped :: (_ -> _ B) -> _ A -> _) "bindFlipped"
--       , val (Simple.Data.Maybe.map :: (A -> B) -> _) "map"
--       , val (Simple.Data.Maybe.pure :: A -> _) "pure"
--       , val (Simple.Data.Maybe.eq :: _ -> _ A -> _) "eq"
--       ]

--   , "Simple.Data.Number" /\
--       [ val (Simple.Data.Number.eq) "eq"
--       ]