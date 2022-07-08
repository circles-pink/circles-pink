module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  , fetchUsersBinarySearch
  ) where

import Prelude

import CirclesCore (TrustNode, User)
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Data.TrustConnection (TrustConnection(..))
import CirclesPink.Data.TrustState (initTrusted, initUntrusted, isLoadingTrust, isLoadingUntrust, isPendingTrust, isPendingUntrust, isTrusted, next)
import CirclesPink.Data.UserIdent (UserIdent(..), getAddress)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', deploySafe', dropError, retryUntil, subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.StateMachine.State.Dashboard (CirclesGraph)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, drop, find, take)
import Data.Array as A
import Data.BN (BN)
import Data.BN as BN
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, isRight, note)
import Data.Foldable (foldM)
import Data.Graph (EitherV)
import Data.Graph.Errors as GE
import Data.Int (floor, toNumber)
import Data.IxGraph as G
import Data.Maybe (Maybe(..))
import Data.Pair ((~))
import Data.String (length)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Extra (todo)
import Foreign.Object (insert)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import RemoteData (RemoteData, _failure, _loading, _success)
import Type.Row (type (+))

type ErrFetchUsersBinarySearch r = (err :: Unit | r)

splitArray :: forall a. Array a -> Array a /\ Array a
splitArray xs =
  let
    count = floor ((toNumber $ A.length xs) / 2.0)
  in
    take count xs /\ drop count xs

fetchUsersBinarySearch :: forall r m. Monad m => Env.Env m -> PrivateKey -> Array Address -> ExceptV (ErrFetchUsersBinarySearch + r) m (Array (Either Address User))
fetchUsersBinarySearch _ _ xs
  | A.length xs == 0 = pure []

fetchUsersBinarySearch env privKey xs
  | A.length xs == 1 = do
      eitherUsers <- lift $ runExceptT $ env.getUsers privKey [] xs
      case eitherUsers of
        Left _ -> pure $ map Left xs
        Right ok -> pure $ map Right ok

fetchUsersBinarySearch env privKey xs = do
  eitherUsers <- lift $ runExceptT $ env.getUsers privKey [] xs
  case eitherUsers of
    Left _ ->
      let
        (ls /\ rs) = splitArray xs
      in
        fetchUsersBinarySearch env privKey ls <> fetchUsersBinarySearch env privKey rs
    Right ok -> pure $ map Right ok

dashboard
  :: forall m
   . MonadCircles m
  => Env.Env m
  -> { logout :: ActionHandler' m Unit S.DashboardState ("landing" :: S.LandingState)
     , getTrusts :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     , addTrustConnection :: ActionHandler' m UserIdent S.DashboardState ("dashboard" :: S.DashboardState)
     , removeTrustConnection :: ActionHandler' m UserIdent S.DashboardState ("dashboard" :: S.DashboardState)
     , getBalance :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     , transfer ::
         ActionHandler' m
           { from :: Address
           , to :: Address
           , value :: BN
           , paymentNote :: String
           }
           S.DashboardState
           ("dashboard" :: S.DashboardState)
     , getUsers ::
         ActionHandler' m
           { userNames :: Array String
           , addresses :: Array Address
           }
           S.DashboardState
           ("dashboard" :: S.DashboardState)
     , userSearch ::
         ActionHandler' m
           { query :: String
           }
           S.DashboardState
           ("dashboard" :: S.DashboardState)
     , redeploySafeAndToken :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     }
dashboard env =
  { logout: \_ _ _ -> pure unit
  , getTrusts
  , addTrustConnection
  , removeTrustConnection
  , getBalance
  , getUsers
  , transfer
  , userSearch
  , redeploySafeAndToken
  }
  where
  getTrusts set st _ =
    void do
      runExceptT do
        _ <-
          syncTrusts set st
            # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
        let x = todo -- infinite
        _ <-
          syncTrusts set st
            # retryUntil env (const { delay: 10000 }) (\_ _ -> false) 0
        pure unit

  -- syncTrusts :: ActionHandler' m Int S.DashboardState ("dashboard" :: S.DashboardState)
  syncTrusts set st i = do
    let
      getNode :: Maybe User -> TrustNode -> UserIdent
      getNode maybeUser tn = UserIdent $ note tn.safeAddress maybeUser

    trustNodes <-
      env.trustGetNetwork st.privKey
        # (\x -> subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r }) x i)
        # dropError
    users <-
      fetchUsersBinarySearch env st.privKey (map _.safeAddress trustNodes)
        # dropError
    let
      foundUsers = catMaybes $ map hush users

    lift
      $ set \st' ->
          let
            ownAddress = st'.user.safeAddress

            newNodes = trustNodes
              <#> (\tn -> tn # getNode (find (\u -> u.safeAddress == tn.safeAddress) foundUsers))

            neigborEdges :: CirclesGraph -> TrustNode -> EitherV (GE.ErrAll Address ()) CirclesGraph
            neigborEdges g tn =
              let
                otherAddress = tn.safeAddress

                outgoingEdge g' = st'.trusts
                  # G.lookupEdge (ownAddress ~ otherAddress)
                  # (\e -> getOutgoingEdge (hush e) ownAddress tn g')

                incomingEdge g' = st'.trusts
                  # G.lookupEdge (ownAddress ~ otherAddress)
                  # (\e -> getIncomingEdge (hush e) ownAddress tn g')
              in
                g # outgoingEdge >>= incomingEdge

            eitherNewTrusts = st'.trusts
              # (G.insertNode (UserIdent $ Right $ st'.user))
              >>= (G.insertNodes newNodes)
              >>= (\g -> foldM neigborEdges g trustNodes)

          in
            case eitherNewTrusts of
              Right newTrusts -> S._dashboard st'
                { trusts = newTrusts
                }
              Left e -> unsafePartial $ crashWith $ GE.printError e

  getOutgoingEdge :: Maybe TrustConnection -> Address -> TrustNode -> CirclesGraph -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getOutgoingEdge maybeOldTrustConnection ownAddress tn g =
    case maybeOldTrustConnection of
      Nothing | tn.isIncoming ->
        G.addEdge (TrustConnection (ownAddress ~ tn.safeAddress) initTrusted) g
      Nothing -> pure g
      Just (TrustConnection _ oldTrustState) | isPendingTrust oldTrustState && tn.isIncoming ->
        G.addEdge (TrustConnection (ownAddress ~ tn.safeAddress) initTrusted) g
      Just (TrustConnection _ oldTrustState) | isPendingUntrust oldTrustState && not tn.isIncoming ->
        g
          # G.deleteEdge (ownAddress ~ tn.safeAddress)
      _ -> pure g

  getIncomingEdge :: Maybe TrustConnection -> Address -> TrustNode -> CirclesGraph -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getIncomingEdge maybeOldTrustConnection ownAddress tn g =
    case maybeOldTrustConnection of
      Nothing | tn.isOutgoing ->
        G.addEdge (TrustConnection (tn.safeAddress ~ ownAddress) initTrusted) g
      Nothing -> Right g
      _ -> G.addEdge (TrustConnection (tn.safeAddress ~ ownAddress) initTrusted) g

  getUsers set st { userNames, addresses } = do
    set \st' -> S._dashboard st' { getUsersResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV (S.ErrGetUsers + ()) _ _
      task = env.getUsers st.privKey userNames addresses
    result <- runExceptT $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { getUsersResult = _failure e }
      Right u -> set \st' -> S._dashboard st' { getUsersResult = _success u }

  addTrustConnection set st u =
    void do
      runExceptT do
        let
          targetAddress = getAddress u

        lift
          $ set \st' ->
              let
                ownAddress = st'.user.safeAddress

                eitherNewTrusts =
                  let
                    eitherNode = G.lookupNode targetAddress st'.trusts
                    eitherEdge = G.lookupEdge (ownAddress ~ targetAddress) st'.trusts
                  in
                    case eitherNode, eitherEdge of
                      Left _, Left _ -> st'.trusts
                        # G.addNode u
                        >>= G.addEdge (TrustConnection (ownAddress ~ targetAddress) (next initUntrusted))
                      Right _, Left _ -> st'.trusts
                        # G.addEdge (TrustConnection (ownAddress ~ targetAddress) (next initUntrusted))
                      _, _ -> Right $ st'.trusts
              in
                case eitherNewTrusts of
                  Right newTrusts -> S._dashboard st'
                    { trusts = newTrusts
                    }
                  Left _ -> S._dashboard st'
        _ <-
          env.addTrustConnection st.privKey targetAddress st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustAddResult = insert (show targetAddress) r st.trustAddResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError

        lift
          $ set \st' ->
              let
                ownAddress = st'.user.safeAddress

                eitherNewTrusts =
                  let
                    eitherNode = G.lookupNode targetAddress st'.trusts
                    eitherEdge = G.lookupEdge (ownAddress ~ targetAddress) st'.trusts
                  in
                    case eitherNode, eitherEdge of
                      Right _, Right (TrustConnection _ edge) -> st'.trusts
                        # G.addEdge (TrustConnection (ownAddress ~ targetAddress) (if isLoadingTrust edge then next edge else edge))
                      _, _ -> Right st'.trusts
              in
                case eitherNewTrusts of
                  Right newTrusts -> S._dashboard st'
                    { trusts = newTrusts
                    }
                  Left _ -> S._dashboard st'
        pure unit

  removeTrustConnection set st u =
    void do
      runExceptT do
        let
          targetAddress = getAddress u

        lift
          $ set \st' ->
              let
                ownAddress = st'.user.safeAddress

                eitherNewTrusts =
                  let
                    eitherNode = G.lookupNode targetAddress st'.trusts
                    eitherEdge = G.lookupEdge (ownAddress ~ targetAddress) st'.trusts
                  in
                    case eitherNode, eitherEdge of
                      Right _, Right (TrustConnection _ edge) -> st'.trusts
                        # G.updateEdge
                            (TrustConnection (ownAddress ~ targetAddress) (if isTrusted edge then next edge else edge))
                      _, _ -> Right st'.trusts
              in
                case eitherNewTrusts of
                  Right newTrusts -> S._dashboard st' { trusts = newTrusts }
                  Left _ -> S._dashboard st'
        _ <-
          env.removeTrustConnection st.privKey targetAddress st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustRemoveResult = insert (show targetAddress) r st.trustRemoveResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError

        lift
          $ set \st' ->
              let
                ownAddress = st'.user.safeAddress

                eitherNewTrusts =
                  let
                    eitherNode = G.lookupNode targetAddress st'.trusts
                    eitherEdge = G.lookupEdge (ownAddress ~ targetAddress) st'.trusts
                  in
                    case eitherNode, eitherEdge of
                      Right _, Right (TrustConnection _ edge) -> st'.trusts
                        # G.updateEdge (TrustConnection (ownAddress ~ targetAddress) (if isLoadingUntrust edge then next edge else edge))
                        # lmap GE.printError
                      _, _ -> Right $ st'.trusts
              in
                case eitherNewTrusts of
                  Right newTrusts -> S._dashboard st' { trusts = newTrusts }
                  Left _ -> S._dashboard st'

        pure unit

  getBalance set st _ =
    void do
      runExceptT do
        _ <-
          env.getBalance st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 2000 }) (\_ n -> n == 3) 0
        checkPayout <-
          env.checkUBIPayout st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { checkUBIPayoutResult = r })
            # retryUntil env (const { delay: 5000 }) (\r n -> n == 5 || isRight r) 0
        when ((length $ BN.toDecimalStr checkPayout) >= 18) do
          env.requestUBIPayout st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { requestUBIPayoutResult = r })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 5 || isRight r) 0
            # void
        _ <-
          env.getBalance st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 2000 }) (\r n -> n == 5 || isRight r) 0
        let x = todo
        -- _ <-
        --   env.getBalance st.privKey st.user.safeAddress
        --     # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
        --     # retryUntil env (const { delay: 10000 }) (\_ _ -> false) 0
        pure unit

  userSearch set st options = do
    void do
      runExceptT do
        env.userSearch st.privKey options
          # (\x -> subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { userSearchResult = r }) x 0)

  transfer set st { from, to, value, paymentNote } = do
    set \st' -> S._dashboard st' { transferResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV (S.ErrTokenTransfer + ()) _ _
      task = env.transfer st.privKey from to value paymentNote
    result <- runExceptT $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { transferResult = _failure e }
      Right h -> set \st' -> S._dashboard st' { transferResult = _success h }

  redeploySafeAndToken set st _ = do
    void do
      runExceptT do
        _ <- deploySafe' env st.privKey
          # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { redeploySafeResult = r })
          # retryUntil env (const { delay: 250 })
              ( \r _ -> case r of
                  Right res -> res.isCreated && res.isDeployed
                  Left _ -> false
              )
              0
          # dropError
        _ <- env.deployToken st.privKey
          # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { redeployTokenResult = r })
          # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
          # dropError
        pure unit