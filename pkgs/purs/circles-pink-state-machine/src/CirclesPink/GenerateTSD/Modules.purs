module CirclesPink.GenerateTSD.Modules where

import Prelude

import CirclesCore as CirclesPink.Data.User
import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.Mnemonic as CirclesPink.Data.Mnemonic
import CirclesPink.Data.PrivateKey.Type as CirclesPink.Data.PrivateKey.Type
import CirclesPink.Data.TrustConnection as CirclesPink.Data.TrustConnection
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.Data.TrustState as CirclesPink.Data.TrustState
import CirclesPink.Data.UserIdent as CirclesPink.Data.UserIdent
import CirclesPink.Garden.StateMachine.Action as CirclesPink.Garden.StateMachine.Action
import CirclesPink.Garden.StateMachine.Control.EnvControl as CirclesPink.Garden.StateMachine.Control.EnvControl
import CirclesPink.Garden.StateMachine.Direction as CirclesPink.Garden.StateMachine.Direction
import CirclesPink.Garden.StateMachine.State as CirclesPink.Garden.StateMachine.State
import CirclesPink.Garden.StateMachine.State.Dashboard as CirclesPink.Garden.StateMachine.State.Dashboard
import CirclesPink.Garden.StateMachine.TrackingEvent as CirclesPink.Garden.StateMachine.TrackingEvent
import CirclesPink.Garden.StateMachine.TrackingResumee as CirclesPink.Garden.StateMachine.TrackingResumee
import CirclesPink.Garden.TS as CirclesPink.Garden.TS
import CirclesPink.GenerateTSD.Replace as R
import CirclesPink.GenerateTSD.TypeClasses (ClassOrd, ORD(..))
import Data.Argonaut as Data.Argonaut
import Data.BN as Data.BN
import Data.DateTime.Instant as Data.DateTime.Instant
import Data.Either (Either)
import Data.Either as Data.Either
import Data.Graph.Diff as Data.Graph.Diff
import Data.Graph.Errors (ErrLookupEdge, ErrNeighborNodes, ErrNeighborhood, ErrLookupNode)
import Data.HTTP.Method as Data.HTTP.Method
import Data.Int as Data.Int
import Data.IxGraph as Data.IxGraph
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Newtype (class Newtype)
import Data.Nullable as Data.Nullable
import Data.Ord as Data.Ord
import Data.Pair as Data.Pair
import Data.Time.Duration as Data.Time.Duration
import Data.Tuple (fst)
import Data.Tuple as Data.Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Effect as Effect
import Foreign.Object as Foreign.Object
import Milkis.Impl as Milkis.Impl
import Network.Ethereum.Core.Signatures as Network.Ethereum.Core.Signatures
import Prelude as Data.Unit
import PursTsGen (classDef, defPredicateFn, instanceDef, pursModule, toTsType, typeDef, value)
import PursTsGen.Class.ToTsType (class ToTsType)
import PursTsGen.Data.ABC (A(..), B(..), C, E, I(..), L, N, Z)
import PursTsGen.Lang.TypeScript.DSL as TS
import RemoteData (RemoteData)
import RemoteData as RemoteData
import RemoteReport as RemoteReport
import Simple.Data.Array as Simple.Data.Array
import Simple.Data.Duration as Simple.Data.Duration
import Simple.Data.Either as Simple.Data.Either
import Simple.Data.Maybe as Simple.Data.Maybe
import Simple.Data.Pair as Simple.Data.Pair
import Simple.Data.Tuple as Simple.Data.Tuple
import Simple.Network.Ethereum.Core.Signatures as Simple.Network.Ethereum.Core.Signatures
import Type.Proxy (Proxy(..))
import VoucherServer.Spec.Types as VoucherServer.Spec.Types

moduleMap :: Map String (String /\ String)
moduleMap = modules <#> (fst >>> pursModule) # M.fromFoldable

_Ord :: forall a. Ord a => ToTsType a => a -> TS.Type
_Ord a = TS.TypeConstructor (TS.QualName (Just "Data_Ord") "Ord") [ toTsType a ]

_Newtype :: forall a b. Newtype a b => ToTsType a => ToTsType b => a -> b -> TS.Type
_Newtype a b = TS.TypeConstructor (TS.QualName (Just "Data_Newtype") "Newtype") [ toTsType a, toTsType b ]

data Any

instance ToTsType Any where
  toTsType _ = TS.any

modules :: Array (String /\ Array TS.Declaration)
modules =
  [ "CirclesPink.Garden.StateMachine.State.Dashboard" /\ join
      [ R.typeAlias "CirclesGraph"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.Dashboard.CirclesGraph)
      , R.typeAlias "VoucherProvidersResult"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.Dashboard.VoucherProvidersResult)
      , R.typeAlias "VouchersResult"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.Dashboard.VouchersResult)
      ]

  , "Data.DateTime.Instant" /\ join
      [ R.typeDef "--"
          (Proxy :: _ Data.DateTime.Instant.Instant)
      , R.value "unInstant" []
          (Data.DateTime.Instant.unInstant)
      ]

  , "Data.Time.Duration" /\ join
      [ R.typeDef "--"
          (Proxy :: _ Data.Time.Duration.Milliseconds)
      ]

  , "Simple.Data.Duration" /\ join
      [ R.value "unMilliseconds" []
          (Simple.Data.Duration.unMilliseconds)
      ]

  , "CirclesPink.Garden.StateMachine.Action" /\ join
      [ R.typeAlias "CirclesAction"
          (Proxy :: _ CirclesPink.Garden.StateMachine.Action.CirclesAction)
      , R.typeAlias "DashboardAction"
          (Proxy :: _ CirclesPink.Garden.StateMachine.Action.DashboardAction)
      , R.typeAlias "DebugAction"
          (Proxy :: _ CirclesPink.Garden.StateMachine.Action.DebugAction)
      , R.value "_circlesAction" []
          (CirclesPink.Garden.StateMachine.Action._circlesAction)
      , R.value "_debugAction" []
          (CirclesPink.Garden.StateMachine.Action._debugAction)
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
      , R.value "_magicWordsAction" []
          (CirclesPink.Garden.StateMachine.Action._magicWordsAction)
      , R.value "_submitAction" []
          (CirclesPink.Garden.StateMachine.Action._submitAction)
      , R.value "_trustsAction" []
          (CirclesPink.Garden.StateMachine.Action._trustsAction)
      , R.value "_dashboardAction" []
          (CirclesPink.Garden.StateMachine.Action._dashboardAction)
      ]

  , "Network.Ethereum.Core.Signatures" /\ join
      [ R.typeDef "--"
          (Proxy :: _ Network.Ethereum.Core.Signatures.Address)
      ]

  , "CirclesPink.Garden.StateMachine.State" /\ join
      [ R.typeAlias "DebugState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.DebugState)
      , R.typeAlias "LandingState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.LandingState)
      , R.typeAlias "LoginState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.LoginState)
      , R.typeAlias "UserData"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.UserData)
      , R.typeAlias "CirclesState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.CirclesState)
      , R.typeAlias "TrustState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.TrustState)
      , R.typeAlias "DashboardState"
          (Proxy :: _ CirclesPink.Garden.StateMachine.State.DashboardState)
      , R.value "initLanding" []
          (CirclesPink.Garden.StateMachine.State.initLanding)
      , R.value "initUserData" []
          (CirclesPink.Garden.StateMachine.State.initUserData)
      , R.value "initDebug" []
          (CirclesPink.Garden.StateMachine.State.initDebug)
      ]

  , "Milkis.Impl" /\ join
      [ R.typeDef "--"
          (Proxy :: _ (Milkis.Impl.FetchImpl))
      ]

  , "Data.HTTP.Method" /\ join
      [ R.typeDef "--"
          (Proxy :: _ (Data.HTTP.Method.Method))
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

  , "CirclesPink.Garden.TS" /\ join
      [ R.typeAlias "CirclesConfigEffect"
          (Proxy :: _ (CirclesPink.Garden.TS.CirclesConfigEffect))
      , R.value "mkControl" []
          (CirclesPink.Garden.TS.mkControl)
      , R.value "mkControlTestEnv" []
          (CirclesPink.Garden.TS.mkControlTestEnv)
      ]

  , "RemoteReport" /\ join
      [ R.value "getData" []
          (RemoteReport.getData :: _ -> RemoteReport.RemoteReport E A -> _)
      ]

  , "Data.IxGraph" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ (Data.IxGraph.IxGraph I E N))
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
        , R.value "edges"
            [ _Ord ORD ]
            (Data.IxGraph.edges :: _ ORD E N -> _)
        , R.value "nodes"
            [ _Ord ORD ]
            (Data.IxGraph.nodes :: _ ORD E N -> _)
        , R.value "toGraph" [] (Data.IxGraph.toGraph :: _ I E N -> _)
        ]

  , "Data.Graph.Diff" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ (Data.Graph.Diff.DiffInstruction I E N))
        , R.value "unDiffInstruction" [] Data.Graph.Diff.unDiffInstruction
        ]

  , "Effect" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ (Effect.Effect A))
        ]

  , "CirclesPink.Data.Address" /\
      join
        [ typeDef "--"
            (Proxy :: _ CirclesPink.Data.Address.Address)
        , instanceDef "ordAddress"
            (_Ord (Proxy :: _ CirclesPink.Data.Address.Address))
        , value "addrToString" []
            CirclesPink.Data.Address.addrToString
        , value "parseAddress" []
            CirclesPink.Data.Address.parseAddress
        , R.value "Address" []
            CirclesPink.Data.Address.Address
        ]

  , "Simple.Network.Ethereum.Core.Signatures" /\
      join
        [ R.value "showAddress" []
            Simple.Network.Ethereum.Core.Signatures.showAddress
        ]

  , "CirclesPink.Data.User" /\
      join
        [ R.typeAlias "User"
            (Proxy :: _ CirclesPink.Data.User.User)
        ]

  , "CirclesPink.Garden.StateMachine.TrackingEvent" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ CirclesPink.Garden.StateMachine.TrackingEvent.TrackingEvent)
        , R.value "encodeJsonTrackingEvent" [] (CirclesPink.Garden.StateMachine.TrackingEvent.encodeJsonTrackingEvent)
        ]

  , "CirclesPink.Garden.StateMachine.TrackingResumee" /\
      join
        [ R.typeAlias "Resumee"
            (Proxy :: _ CirclesPink.Garden.StateMachine.TrackingResumee.Resumee)
        , R.value "init" [] (CirclesPink.Garden.StateMachine.TrackingResumee.init)
        , R.value "encodeJsonResumee" [] (CirclesPink.Garden.StateMachine.TrackingResumee.encodeJsonResumee)
        , R.value "decodeJsonResumee" [] (CirclesPink.Garden.StateMachine.TrackingResumee.decodeJsonResumee)
        ]

  , "CirclesPink.Garden.StateMachine.Direction" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ CirclesPink.Garden.StateMachine.Direction.Direction)
        , R.value "unDirection" [] (CirclesPink.Garden.StateMachine.Direction.unDirection :: _ -> _ -> Z)
        ]

  , "CirclesPink.Data.PrivateKey.Type" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ CirclesPink.Data.PrivateKey.Type.PrivateKey)
        ]

  , "Data.Argonaut" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ Data.Argonaut.Json)
        , R.typeDef "--"
            (Proxy :: _ Data.Argonaut.JsonDecodeError)
        ]

  , "Data.Int" /\
      join
        [ value "toNumber" []
            Data.Int.toNumber
        ]

  , "CirclesPink.Data.Mnemonic" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ CirclesPink.Data.Mnemonic.Mnemonic)
        , value "getWords" []
            CirclesPink.Data.Mnemonic.getWords
        , value "keyToMnemonic" []
            CirclesPink.Data.Mnemonic.keyToMnemonic
        ]

  , "VoucherServer.Spec.Types" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ VoucherServer.Spec.Types.VoucherAmount)
        , R.typeDef "--"
            (Proxy :: _ VoucherServer.Spec.Types.VoucherProviderId)
        , R.typeAlias "VoucherProvider"
            (Proxy :: _ VoucherServer.Spec.Types.VoucherProvider)
        , R.typeAlias "VoucherOffer"
            (Proxy :: _ VoucherServer.Spec.Types.VoucherOffer)
        , R.typeAlias "Voucher"
            (Proxy :: _ VoucherServer.Spec.Types.Voucher)
        , value "unVoucherProviderId" []
            VoucherServer.Spec.Types.unVoucherProviderId
        , value "unVoucherAmount" []
            VoucherServer.Spec.Types.unVoucherAmount
        , value "unVoucherCode" []
            VoucherServer.Spec.Types.unVoucherCode
        ]

  , "Foreign.Object" /\
      join
        [ R.typeDef "--"
            (Proxy :: _ (Foreign.Object.Object A))
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
        , value "initTrustNode" []
            CirclesPink.Data.TrustNode.initTrustNode
        , instanceDef "ordTrustNode"
            (_Ord (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode))
        , value "getAddress" []
            CirclesPink.Data.TrustNode.getAddress
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

  , "Simple.Data.Either" /\
      join
        [ value "unEither" []
            (Simple.Data.Either.unEither :: _ -> _ A B -> Z)
        , value "mkEither" []
            ( Simple.Data.Either.mkEither
                :: { mkLeft :: A -> _
                   , mkRight :: B -> _
                   }
            )
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

  , "Simple.Data.Pair" /\
      join
        [ R.value "unPair" []
            (Simple.Data.Pair.unPair :: _ -> _ A -> Z)
        , R.value "mkPair" []
            (Simple.Data.Pair.mkPair :: A -> _)
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
        , value "mkMaybe" []
            (Simple.Data.Maybe.mkMaybe :: { mkJust :: A -> _, mkNothing :: _ })
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