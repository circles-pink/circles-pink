module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  , fetchUsersBinarySearch
  , spec
  ) where

import CirclesPink.Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (Address(..), parseAddress)
import CirclesPink.Data.PrivateKey (PrivateKey, sampleKey)
import CirclesPink.Data.TrustConnection (TrustConnection(..))
import CirclesPink.Data.TrustNode (TrustNode(..), initTrustNode)
import CirclesPink.Data.TrustNode as TN
import CirclesPink.Data.TrustState (initTrusted, initUntrusted, isLoadingTrust, isLoadingUntrust, isPendingTrust, isPendingUntrust, isTrusted, next)
import CirclesPink.Data.User (User)
import CirclesPink.Data.UserIdent (UserIdent(..), UserIdent', getAddress)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', deploySafe', dropError, retryUntil, subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Control.EnvControl as EnvControl
import CirclesPink.Garden.StateMachine.State (DashboardState)
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.StateMachine.State.Dashboard (CirclesGraph)
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, drop, take)
import Data.Array as A
import Data.BN (BN)
import Data.BN as BN
import Data.DateTime.Instant (unInstant)
import Data.Graph (EitherV)
import Data.Graph.Errors as GE
import Data.Identity (Identity)
import Data.Int as Int
import Data.IxGraph (getIndex)
import Data.IxGraph as G
import Data.Lens (Lens', Traversal', set, traversed)
import Data.Lens as L
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number.Format as N
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
      apiUsersIds = Set.fromFoldable $ (\u -> u -# _.safeAddress) <$> apiUsers
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
        env.getUsers privKey [] xs # withExceptT (const (inj (Proxy :: Proxy "err") unit))

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
        (Right [ UserIdent $ Right $ userA ])

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
        (Right [ UserIdent $ Left addrB, UserIdent $ Right $ userA ])

--------------------------------------------------------------------------------

dashboard
  :: forall m
   . MonadCircles m
  => EnvControl m
  -> { logout :: ActionHandler' m Unit S.DashboardState ("landing" :: S.LandingState)
     , getTrusts :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     , expandTrustNetwork :: ActionHandler' m _ S.DashboardState ("dashboard" :: S.DashboardState)
     , addTrustConnection :: ActionHandler' m UserIdent S.DashboardState ("dashboard" :: S.DashboardState)
     , removeTrustConnection :: ActionHandler' m UserIdent S.DashboardState ("dashboard" :: S.DashboardState)
     , getVouchers :: ActionHandler' m String S.DashboardState ("dashboard" :: S.DashboardState)
     , getVoucherProviders :: HandlerGetVoucherProviders m
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
  , getVouchers
  , getVoucherProviders: getVoucherProviders env
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
          syncTrusts set st (st.user -# _.safeAddress)
            # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
        -- let x = todo -- infinite
        -- _ <- lift $ env.sleep 5000
        -- _ <-
        --   syncTrusts set st st.user.safeAddress
        --     # retryUntil env (const { delay: 5000 }) (\_ _ -> false) 0
        pure unit

  expandTrustNetwork set st safeAddress =
    void $ runExceptT do
      let
        _nodeIsLoading = prop _trusts
          <<< G._atNode safeAddress
          <<< traversed
          <<< _Newtype
          <<< prop _isLoading

      lift $ set $ S._dashboard <<< L.set _nodeIsLoading true

      syncTrusts set st safeAddress 0 `catchError`
        ( const $ do
            lift $ env.sleep 2000
            lift $ set $ S._dashboard <<< L.set _nodeIsLoading false
        )
      lift $ env.sleep 2000
      lift $ set $ S._dashboard <<< L.set _nodeIsLoading false
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
                ownAddress = st'.user -# _.safeAddress

                eitherNewTrusts =
                  let
                    eitherNode = G.lookupNode targetAddress st'.trusts
                    eitherEdge = G.lookupEdge (ownAddress ~ targetAddress) st'.trusts
                  in
                    case eitherNode, eitherEdge of
                      Left _, Left _ -> st'.trusts
                        # G.addNode (initTrustNode u)
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
          env.addTrustConnection st.privKey targetAddress (st.user -# _.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustAddResult = insert (show targetAddress) r st.trustAddResult })
            # retryUntil env (const { delay: 1000 }) (\r n -> n == 10 || isRight r) 0
            # dropError

        lift
          $ set \st' ->
              let
                ownAddress = st'.user -# _.safeAddress

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
                ownAddress = st'.user -# _.safeAddress

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
          env.removeTrustConnection st.privKey targetAddress (st.user -# _.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustRemoveResult = insert (show targetAddress) r st.trustRemoveResult })
            # retryUntil env (const { delay: 1000 }) (\r n -> n == 10 || isRight r) 0
            # dropError

        lift
          $ set \st' ->
              let
                ownAddress = st'.user -# _.safeAddress

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

  getVouchers set st msg =
    void do
      runExceptT do
        signatureObj <- lift $ env.signChallenge (Message msg) st.privKey
        _ <-
          env.getVouchers signatureObj
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { vouchersResult = r })
            # retryUntil env (const { delay: 1000 }) (\_ n -> n == 0) 0
        pure unit

  getBalance set st _ =
    void do
      runExceptT do
        _ <-
          env.getBalance st.privKey (st.user -# _.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
        pure unit

  getUBIPayout set st _ =
    void do
      runExceptT do
        checkPayout <-
          env.checkUBIPayout st.privKey (st.user -# _.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { checkUBIPayoutResult = r })
            # retryUntil env (const { delay: 5000 }) (\r n -> n == 5 || isRight r) 0
        when ((Str.length $ BN.toDecimalStr checkPayout) >= 18) do
          env.requestUBIPayout st.privKey (st.user -# _.safeAddress)
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

    trustNodes :: Map Address CC.TrustNode <-
      trustGetNetwork st.privKey centerAddress
        # (\x -> subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r }) x i)
        # dropError
        <#> map (\v -> wrap v.safeAddress /\ v)
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
                # (G.insertNode (initTrustNode $ UserIdent $ Right $ st'.user))
                >>= (\g -> foldM getNode g $ mapsToThese userIdents neighborNodes)
                >>= (\g -> foldM (getIncomingEdge centerAddress) g $ mapsToThese trustNodes incomingEdges)
                >>= (\g -> foldM (getOutgoingEdge centerAddress) g $ mapsToThese trustNodes outgoingEdges)

          in
            case eitherNewTrusts of
              Right newTrusts -> S._dashboard st'
                { trusts = newTrusts
                }
              Left e -> unsafePartial $ crashWith $ GE.printError e

  getNode :: CirclesGraph -> These UserIdent TrustNode -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getNode g =
    case _ of
      This uiApi -> g # G.insertNode (TN.initTrustNode uiApi) -- use addNode
      That _ -> Right g
      Both uiApi _ -> G.modifyNode (getIndex uiApi) (set TN._userIdent uiApi) g

  getOutgoingEdge :: Address -> CirclesGraph -> These CC.TrustNode TrustConnection -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getOutgoingEdge centerAddress g = case _ of
    This tn | tn.isIncoming -> G.addEdge (TrustConnection (centerAddress ~ wrap tn.safeAddress) initTrusted) g
    This _ -> Right g
    That (TrustConnection _ ts) | isPendingTrust ts || isLoadingTrust ts -> Right g
    That tc -> G.deleteEdge (getIndex tc) g
    Both tn (TrustConnection _ ts) | tn.isIncoming && isPendingTrust ts -> G.updateEdge (TrustConnection (centerAddress ~ wrap tn.safeAddress) initTrusted) g
    Both tn tc@(TrustConnection _ ts) | not tn.isIncoming && isPendingUntrust ts -> G.deleteEdge (getIndex tc) g
    Both _ _ -> Right g

  getIncomingEdge :: Address -> CirclesGraph -> These CC.TrustNode TrustConnection -> EitherV (GE.ErrAll Address ()) CirclesGraph
  getIncomingEdge centerAddress g = case _ of
    This tn | tn.isOutgoing -> G.addEdge (TrustConnection (wrap tn.safeAddress ~ centerAddress) initTrusted) g
    This _ -> Right g
    That tc -> G.deleteEdge (getIndex tc) g
    Both tn _ -> G.updateEdge (TrustConnection (wrap tn.safeAddress ~ centerAddress) initTrusted) g

--------------------------------------------------------------------------------

type HandlerGetVoucherProviders m = ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)

getVoucherProviders :: forall m. MonadCircles m => EnvControl m -> HandlerGetVoucherProviders m
getVoucherProviders env set st _ =
  void do
    runExceptT do
      msg <- env.getTimestamp # lift <#> unInstant >>> unwrap >>> N.toString
      signatureObj <- lift $ env.signChallenge (Message msg) st.privKey
      _ <-
        env.getVoucherProviders signatureObj
          # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { voucherProvidersResult = r })
          # retryUntil env (const { delay: 1000 }) (\_ n -> n == 0) 0
      pure unit

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

_trusts = Proxy :: _ "trusts"

_isLoading = Proxy :: _ "isLoading"
