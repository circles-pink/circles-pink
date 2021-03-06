module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  , fetchUsersBinarySearch
  , spec
  ) where

import CirclesPink.Prelude

import CirclesCore (TrustNode, User)
import CirclesPink.Data.Address (Address, parseAddress)
import CirclesPink.Data.PrivateKey (PrivateKey, sampleKey)
import CirclesPink.Data.TrustConnection (TrustConnection(..))
import CirclesPink.Data.TrustState (initTrusted, initUntrusted, isLoadingTrust, isLoadingUntrust, isPendingTrust, isPendingUntrust, isTrusted, next)
import CirclesPink.Data.UserIdent (UserIdent(..), getAddress)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', deploySafe', dropError, retryUntil, subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Control.EnvControl as EnvControl
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.StateMachine.State.Dashboard (CirclesGraph)
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, drop, take)
import Data.Array as A
import Data.BN (BN)
import Data.BN as BN
import Data.Graph (EitherV)
import Data.Graph.Errors as GE
import Data.Identity (Identity)
import Data.Int as Int
import Data.IxGraph (getIndex)
import Data.IxGraph as G
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Pair ((~))
import Data.Pair as P
import Data.Set as Set
import Data.String as Str
import Data.These (These(..), maybeThese)
import Foreign.Object (insert)
import RemoteData (RemoteData, _failure, _loading, _success)
import Test.TestUtils (addrA, addrB, userA)
import Web3 (Message(..))

--------------------------------------------------------------------------------

splitArray :: forall a. Array a -> Array a /\ Array a
splitArray xs =
  let
    count = Int.floor ((Int.toNumber $ A.length xs) / 2.0)
  in
    take count xs /\ drop count xs

specSplitArray :: Spec Unit
specSplitArray = describeFn splitArray do
  describe "Split empty array" do
    it "returns empty arrays" do
      splitArray ([] :: _ Int) `shouldEqual` ([] /\ [])
  describe "split array of even length" do
    it "returns arrays of same length" do
      splitArray ([ 1, 2, 3, 4 ]) `shouldEqual` ([ 1, 2 ] /\ [ 3, 4 ])
  describe "split array of odd length" do
    it "returns arrays of different length, the latter having one item more" do
      splitArray ([ 1, 2, 3, 4, 5 ]) `shouldEqual` ([ 1, 2 ] /\ [ 3, 4, 5 ])

--------------------------------------------------------------------------------

type ErrFetchUsersBinarySearch r = (err :: Unit | r)

type EnvFetchUsersBinarySearch r m = (getUsers :: EnvControl.GetUsers m | r)

fetchUsersBinarySearch :: forall q r m. Monad m => { | EnvFetchUsersBinarySearch q m } -> PrivateKey -> Array Address -> ExceptV (ErrFetchUsersBinarySearch r) m (Array UserIdent)
fetchUsersBinarySearch env privKey addresses =
  do
    apiUsers <- go addresses
    let
      allIds = Set.fromFoldable addresses
      apiUsersIds = Set.fromFoldable $ (\u -> u.safeAddress) <$> apiUsers
      otherUsersIds = Set.difference allIds apiUsersIds
      otherUsers' = Left <$> (Set.toUnfoldable $ otherUsersIds)
      apiUsers' = Right <$> apiUsers

    pure $ UserIdent <$> (otherUsers' <> apiUsers')

  where
  go :: Array Address -> ExceptV (ErrFetchUsersBinarySearch + r) m (Array User)
  go xs
    | A.length xs == 0 = pure []

  go xs
    | A.length xs == 1 = do
        users <- env.getUsers privKey [] xs # withExceptT (const (inj (Proxy :: Proxy "err") unit))
        pure users

  go xs = do
    eitherUsers <- lift $ runExceptT $ env.getUsers privKey [] xs
    case eitherUsers of
      Left _ ->
        let
          (ls /\ rs) = splitArray xs
        in
          go ls <> go rs
      Right ok -> pure ok

specFetchUsersBinarySearch :: Spec Unit
specFetchUsersBinarySearch = describe "fetchUsersBinarySearch" do
  let
    fetchUsersBinarySearch' = fetchUsersBinarySearch :: { | EnvFetchUsersBinarySearch () _ } -> _ -> _ -> _ (_ (ErrFetchUsersBinarySearch ())) Identity _
    run = runExceptT >>> unwrap

  describe "A" do
    it ".." do
      shouldEqual
        ( run $ fetchUsersBinarySearch'
            { getUsers: \_ _ _ -> pure [] }
            sampleKey
            []
        )
        (Right [])

  describe "B" do
    it ".." do
      shouldEqual
        ( run $ fetchUsersBinarySearch'
            { getUsers: \_ _ _ -> pure [ userA ] }
            sampleKey
            [ addrA ]
        )
        (Right [ UserIdent $ Right userA ])

  describe "C" do
    it ".." do
      shouldEqual
        ( run $ fetchUsersBinarySearch'
            { getUsers: \_ _ _ -> pure [] }
            sampleKey
            [ addrA ]
        )
        (Right [ UserIdent $ Left addrA ])

  describe "C" do
    it ".." do
      shouldEqual
        ( run $ fetchUsersBinarySearch'
            { getUsers: \_ _ _ -> pure [ userA ] }
            sampleKey
            [ addrA, addrB ]
        )
        (Right [ UserIdent $ Left addrB, UserIdent $ Right userA ])

--------------------------------------------------------------------------------

dashboard
  :: forall m
   . MonadCircles m
  => EnvControl m
  -> { logout :: ActionHandler' m Unit S.DashboardState ("landing" :: S.LandingState)
     , getTrusts :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     , expandTrustNetwork :: ActionHandler' m String S.DashboardState ("dashboard" :: S.DashboardState)
     , addTrustConnection :: ActionHandler' m UserIdent S.DashboardState ("dashboard" :: S.DashboardState)
     , removeTrustConnection :: ActionHandler' m UserIdent S.DashboardState ("dashboard" :: S.DashboardState)
     , signMessage :: ActionHandler' m String S.DashboardState ("dashboard" :: S.DashboardState)
     , getBalance :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     , getUBIPayout :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
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
dashboard env@{ trustGetNetwork } =
  { logout: \_ _ _ -> pure unit
  , getTrusts
  , expandTrustNetwork
  , addTrustConnection
  , removeTrustConnection
  , signMessage
  , getBalance
  , getUBIPayout
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
          syncTrusts set st st.user.safeAddress
            # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
        -- let x = todo -- infinite
        -- _ <- lift $ env.sleep 5000
        -- _ <-
        --   syncTrusts set st st.user.safeAddress
        --     # retryUntil env (const { delay: 5000 }) (\_ _ -> false) 0
        pure unit

  expandTrustNetwork set st safeAddress =
    void do
      runExceptT do
        let maybeAddress = parseAddress safeAddress
        case maybeAddress of
          Nothing -> pure unit
          Just addr -> syncTrusts set st addr 0
        pure unit

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
                        # G.updateEdge (TrustConnection (ownAddress ~ targetAddress) (if isLoadingTrust edge then next edge else edge))
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
                      _, _ -> Right $ st'.trusts
              in
                case eitherNewTrusts of
                  Right newTrusts -> S._dashboard st' { trusts = newTrusts }
                  Left _ -> S._dashboard st'

        pure unit

  signMessage _ st msg = do
    _ <- env.signChallenge (Message msg) st.privKey
    -- let _ = spy "signature" sig
    pure unit

  getBalance set st _ =
    void do
      runExceptT do
        _ <-
          env.getBalance st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
        -- checkPayout <-
        --   env.checkUBIPayout st.privKey st.user.safeAddress
        --     # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { checkUBIPayoutResult = r })
        --     # retryUntil env (const { delay: 5000 }) (\r n -> n == 5 || isRight r) 0
        -- when ((length $ BN.toDecimalStr checkPayout) >= 18) do
        --   env.requestUBIPayout st.privKey st.user.safeAddress
        --     # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { requestUBIPayoutResult = r })
        --     # retryUntil env (const { delay: 10000 }) (\r n -> n == 5 || isRight r) 0
        --     # void
        -- _ <-
        --   env.getBalance st.privKey st.user.safeAddress
        --     # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
        --     # retryUntil env (const { delay: 2000 }) (\r n -> n == 5 || isRight r) 0
        -- let x = todo
        -- _ <-
        --   env.getBalance st.privKey st.user.safeAddress
        --     # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
        --     # retryUntil env (const { delay: 10000 }) (\_ _ -> false) 0
        pure unit

  getUBIPayout set st _ =
    void do
      runExceptT do
        checkPayout <-
          env.checkUBIPayout st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { checkUBIPayoutResult = r })
            # retryUntil env (const { delay: 5000 }) (\r n -> n == 5 || isRight r) 0
        when ((Str.length $ BN.toDecimalStr checkPayout) >= 18) do
          env.requestUBIPayout st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { requestUBIPayoutResult = r })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 5 || isRight r) 0
            # void

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

  syncTrusts set st centerAddress i = do

    trustNodes :: Map Address TrustNode <-
      trustGetNetwork st.privKey centerAddress
        # (\x -> subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r }) x i)
        # dropError
        <#> map (\v -> v.safeAddress /\ v)
        >>> M.fromFoldable

    userIdents :: Map Address UserIdent <-
      fetchUsersBinarySearch env st.privKey (Set.toUnfoldable $ M.keys trustNodes)
        # dropError
        <#> map (\v -> getIndex v /\ v)
        >>> M.fromFoldable

    lift
      $ set \st' ->
          let
            eitherNewTrusts :: EitherV (GE.ErrAll Address ()) CirclesGraph
            eitherNewTrusts = do
              let
                neighborNodes = G.neighborNodes centerAddress st'.trusts
                  <#> map (\v -> getIndex v /\ v)
                  >>> M.fromFoldable
                  # either (const M.empty) identity

                incomingEdges = G.incomingEdges centerAddress st'.trusts
                  <#> map (\v -> P.fst (getIndex v) /\ v)
                  >>> M.fromFoldable
                  # either (const M.empty) identity

                outgoingEdges = G.outgoingEdges centerAddress st'.trusts
                  <#> map (\v -> P.snd (getIndex v) /\ v)
                  >>> M.fromFoldable
                  # either (const M.empty) identity

              st'.trusts
                # (G.insertNode (UserIdent $ Right $ st'.user))
                >>= (\g -> foldM getNode g $ mapsToThese userIdents neighborNodes)
                >>= (\g -> foldM (getIncomingEdge centerAddress) g $ mapsToThese trustNodes incomingEdges)
                >>= (\g -> foldM (getOutgoingEdge centerAddress) g $ mapsToThese trustNodes outgoingEdges)

          in
            case eitherNewTrusts of
              Right newTrusts -> S._dashboard st'
                { trusts = newTrusts
                }
              Left e -> unsafePartial $ crashWith $ GE.printError e

  getNode :: CirclesGraph -> These UserIdent UserIdent -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getNode g =
    case _ of
      This uiApi -> G.insertNode uiApi g -- use addNode
      That _ -> Right g
      Both uiApi _ -> G.updateNode uiApi g

  getOutgoingEdge :: Address -> CirclesGraph -> These TrustNode TrustConnection -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getOutgoingEdge centerAddress g = case _ of
    This tn | tn.isIncoming -> G.addEdge (TrustConnection (centerAddress ~ tn.safeAddress) initTrusted) g
    This _ -> Right g
    That (TrustConnection _ ts) | isPendingTrust ts || isLoadingTrust ts -> Right g
    That tc -> G.deleteEdge (getIndex tc) g
    Both tn (TrustConnection _ ts) | tn.isIncoming && isPendingTrust ts -> G.updateEdge (TrustConnection (centerAddress ~ tn.safeAddress) initTrusted) g
    Both tn tc@(TrustConnection _ ts) | not tn.isIncoming && isPendingUntrust ts -> G.deleteEdge (getIndex tc) g
    Both _ _ -> Right g

  getIncomingEdge :: Address -> CirclesGraph -> These TrustNode TrustConnection -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getIncomingEdge centerAddress g = case _ of
    This tn | tn.isOutgoing -> G.addEdge (TrustConnection (tn.safeAddress ~ centerAddress) initTrusted) g
    This _ -> Right g
    That tc -> G.deleteEdge (getIndex tc) g
    Both tn _ -> G.updateEdge (TrustConnection (tn.safeAddress ~ centerAddress) initTrusted) g

--------------------------------------------------------------------------------

mapsToThese :: forall k a b. Ord k => Map k a -> Map k b -> Array (These a b)
mapsToThese mapA mapB = Set.union (M.keys mapA) (M.keys mapB)
  # Set.toUnfoldable
  <#> (\k -> maybeThese (M.lookup k mapA) (M.lookup k mapB))
  # catMaybes

specMapsToThese :: Spec Unit
specMapsToThese =
  describe "mapsToThese" do
    let
      fromFoldable :: Array (String /\ Int) -> Map String Int
      fromFoldable = M.fromFoldable

    describe "empty maps" do
      let
        map1 = fromFoldable []
        map2 = fromFoldable []
      it "returns an empty array" do
        (mapsToThese map1 map2) `shouldEqual` []
    describe "first one entry" do
      let
        map1 = fromFoldable [ "1" /\ 1 ]
        map2 = fromFoldable []
      it "returns an array with one This" do
        (mapsToThese map1 map2) `shouldEqual` [ This 1 ]
    describe "second one entry" do
      let
        map1 = fromFoldable []
        map2 = fromFoldable [ "2" /\ 2 ]
      it "returns an array with one That" do
        (mapsToThese map1 map2) `shouldEqual` [ That 2 ]
    describe "both entries, different keys" do
      let
        map1 = fromFoldable [ "1" /\ 1 ]
        map2 = fromFoldable [ "2" /\ 2 ]
      it "returns an array with one This and one That" do
        (mapsToThese map1 map2) `shouldEqual` [ This 1, That 2 ]
    describe "both entries, same keys" do
      let
        map1 = fromFoldable [ "1" /\ 1 ]
        map2 = fromFoldable [ "1" /\ 2 ]
      it "returns an array with one Both" do
        (mapsToThese map1 map2) `shouldEqual` [ Both 1 2 ]

--------------------------------------------------------------------------------

spec :: Spec Unit
spec =
  describe "CirclesPink.Garden.StateMachine.Control.States.Dashboard" do
    specSplitArray
    specFetchUsersBinarySearch
    specMapsToThese