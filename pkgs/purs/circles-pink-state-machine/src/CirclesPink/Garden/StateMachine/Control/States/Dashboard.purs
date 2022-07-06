module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  , fetchUsersBinarySearch
  ) where

import Prelude

import CirclesCore (TrustNode, User)
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Data.TrustEntry (isConfirmed)
import CirclesPink.Data.TrustState (TrustState, initTrusted, initUntrusted, isLoadingTrust, isLoadingUntrust, isPendingTrust, isPendingUntrust, isTrusted, next)
import CirclesPink.Data.UserIdent (UserIdent(..), getAddress, getIdentifier)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', deploySafe', dropError, retryUntil, subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.StateMachine.State.Dashboard (CirclesGraph)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, drop, find, foldr, take)
import Data.Array as A
import Data.BN (BN)
import Data.BN as BN
import Data.Either (Either(..), hush, isRight, note)
import Data.Int (floor, toNumber)
import Data.IxGraph (getIndex, toUnfoldables)
import Data.IxGraph as G
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (length)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy, spyWith)
import Debug.Extra (todo)
import Foreign.Object (insert)
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
   . Monad m
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
        let x = todo
        _ <-
          syncTrusts set st
            # retryUntil env (const { delay: 10000 }) (\_ _ -> false) 0
        pure unit

  syncTrusts set st i = do
    let
      getNode :: Maybe User -> TrustNode -> UserIdent
      getNode maybeUser tn = UserIdent $ note tn.safeAddress maybeUser

      ownAddress = st.user.safeAddress

      getOutgoingEdge :: Maybe TrustState -> TrustNode -> CirclesGraph -> CirclesGraph
      getOutgoingEdge maybeOldTrustState tn g =
        let
          x = spy "sa" tn.safeAddress 
        in
        case maybeOldTrustState of
          Nothing | tn.isIncoming -> 
            spyWith "1" (const "1") $ G.insertEdge ownAddress tn.safeAddress initTrusted g
          Nothing ->  
            spyWith "2" (const "2") $ g
          Just oldTrustState | isPendingTrust oldTrustState && tn.isIncoming -> 
            spyWith "3" (const "3") $ G.insertEdge ownAddress tn.safeAddress initTrusted g
          Just oldTrustState | isPendingUntrust oldTrustState && not tn.isIncoming -> 
            g 
              # spyWith "4 A" (toUnfoldables :: _ -> {nodes:: Array _ | _}) 
              # G.deleteEdges [ ownAddress /\ tn.safeAddress ]
              # spyWith "4 B" (toUnfoldables :: _ -> {nodes:: Array _ | _})
              
          _ ->
            spyWith "5" (const "5") $ g

      getIncomingEdge :: Maybe TrustState -> TrustNode -> CirclesGraph -> CirclesGraph
      getIncomingEdge maybeOldTrustState tn g =
        case maybeOldTrustState of
          Nothing | tn.isOutgoing -> G.insertEdge tn.safeAddress ownAddress  initTrusted g
          Nothing -> G.deleteEdges [ tn.safeAddress /\ ownAddress  ] g
          _ -> G.insertEdge tn.safeAddress ownAddress  initTrusted g

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

            foo :: TrustNode -> CirclesGraph -> CirclesGraph
            foo tn g =
              let
                otherAddress = tn.safeAddress

                outgoingEdge g' = st'.trusts
                  # G.lookupEdge ownAddress otherAddress
                  # (\e -> getOutgoingEdge e tn g')

                incomingEdge g' = st'.trusts
                  # G.lookupEdge otherAddress ownAddress
                  # (\e -> getIncomingEdge e tn g')

              in
                g # outgoingEdge # incomingEdge

          -- newEdges :: Array (Address /\ Address /\ TrustState)
          -- newEdges = trustNodes
          --   >>=
          --     ( \tn ->
          --         let
          --           otherAddress = tn.safeAddress
          --           outgoingEdges = st'.trusts
          --             # G.lookupEdge ownAddress otherAddress
          --             # (\e -> getOutgoingEdge e tn)
          --             # maybe [] (\x -> [ ownAddress /\ otherAddress /\ x ])

          --           incomingEdges = st'.trusts
          --             # G.lookupEdge otherAddress ownAddress
          --             # (\e -> getIncomingEdge e tn)
          --             # maybe [] (\x -> [ otherAddress /\ ownAddress /\ x ])

          --         in
          --           outgoingEdges <> incomingEdges
          --     )
          in
            S._dashboard
              st'
                { trusts = st'.trusts
                    # G.insertNode (UserIdent $ Right $ st'.user)
                    -- #
                    --   ( \g ->
                    --       let
                    --         ids = g
                    --           # G.outgoingIds ownAddress
                    --           # maybe Set.empty identity
                    --           # Set.filter (\id -> g # G.lookupEdge ownAddress id # maybe false isTrusted)
                    --           # Set.map (\to -> ownAddress /\ to)
                    --       in
                    --         G.deleteEdges ids g
                    --   )
                    # G.insertNodes newNodes
                    # (\g -> foldr foo g trustNodes)
                }

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
              in
                S._dashboard
                  st'
                    { trusts =
                        let
                          maybeNode = G.lookupNode targetAddress st'.trusts
                          maybeEdge = G.lookupEdge ownAddress targetAddress st'.trusts
                        in
                          case maybeNode, maybeEdge of
                            Nothing, Nothing -> st'.trusts
                              # G.insertNode u
                              # G.insertEdge ownAddress targetAddress (next initUntrusted)
                            Just _, Nothing -> st'.trusts
                              # G.insertEdge ownAddress targetAddress (next initUntrusted)
                            _, _ -> st'.trusts
                    }
        _ <-
          env.addTrustConnection st.privKey targetAddress st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustAddResult = insert (show targetAddress) r st.trustAddResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError

        lift
          $ set \st' ->
              let
                ownAddress = st'.user.safeAddress
              in
                S._dashboard
                  st'
                    { trusts =
                        let
                          maybeNode = G.lookupNode targetAddress st'.trusts
                          maybeEdge = G.lookupEdge ownAddress targetAddress st'.trusts
                        in
                          case maybeNode, maybeEdge of
                            Just _, Just edge -> st'.trusts
                              # G.insertEdge ownAddress targetAddress (if isLoadingTrust edge then next edge else edge)
                            _, _ -> st'.trusts
                    }
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
              in
                S._dashboard
                  st'
                    { trusts =
                        let
                          maybeNode = G.lookupNode targetAddress st'.trusts
                          maybeEdge = G.lookupEdge ownAddress targetAddress st'.trusts
                        in
                          case maybeNode, maybeEdge of
                            Just _, Just edge -> st'.trusts
                              # G.insertEdge ownAddress targetAddress (if isTrusted edge then next edge else edge)
                            _, _ -> st'.trusts
                    }
        _ <-
          env.removeTrustConnection st.privKey targetAddress st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustRemoveResult = insert (show targetAddress) r st.trustRemoveResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError

        lift
          $ set \st' ->
              let
                ownAddress = st'.user.safeAddress
              in
                S._dashboard
                  st'
                    { trusts =
                        let
                          maybeNode = G.lookupNode targetAddress st'.trusts
                          maybeEdge = G.lookupEdge ownAddress targetAddress st'.trusts
                        in
                          case maybeNode, maybeEdge of
                            Just _, Just edge -> st'.trusts
                              # G.insertEdge ownAddress targetAddress (if isLoadingUntrust edge then next edge else edge)
                            _, _ -> st'.trusts
                    }
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