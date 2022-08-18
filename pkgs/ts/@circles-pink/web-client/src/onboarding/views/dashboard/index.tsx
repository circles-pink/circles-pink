import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import React, {
  ReactElement,
  SetStateAction,
  useContext,
  useEffect,
  useMemo,
  useState,
} from 'react';
import { Button, Input } from '../../../components/forms';
import { Text } from '../../../components/text';
import { UserDashboard } from '../../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import { DashboardState } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import {
  DefaultView,
  defaultView,
  Trust,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { getIncrementor } from '../../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../../context/theme';
import tw, { css, styled } from 'twin.macro';
import {
  mdiCashFast,
  mdiGiftOutline,
  mdiGraphOutline,
  mdiHandCoin,
  mdiLan,
  mdiMagnify,
} from '@mdi/js';
import { TrustUserList } from '../../../components/TrustUserList';
import { Overlay } from '../../../components/Overlay';
import {
  JustifyBetweenCenter,
  MarginY,
  TwoButtonRow,
} from '../../../components/helper';
import { Send, SendProps } from './Send';
import { Receive } from './Receive';
import { Balance } from './Balance';
import {
  TrustNode,
  User,
} from '@circles-pink/state-machine/output/CirclesCore';
import { StateMachineDebugger } from '../../../components/StateMachineDebugger';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { TrustGraph } from '../../../components/TrustGraph/index';
import { UserSearch } from '../../../components/UserSearch';
import { ListVouchers } from './ListVouchers';
import { BuyVouchers } from './BuyVouchers';
import {
  VoucherOffer,
  VoucherProvider,
} from '@circles-pink/state-machine/output/VoucherServer.Spec.Types';
import { ConfirmSend } from './ConfirmSend';
import { displayBalance } from '../../utils/timeCircles';
import { UserConfig } from '../..';
import { LightColorFrame } from '../../../components/layout';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

export type MappedTrustNodes = Array<TrustNode & User>;

export type UserData = {
  username: string;
  avatarUrl: string | null;
};

export type Overlay = 'SEND' | 'RECEIVE' | 'CONFIRM_SEND';

export type SelectedOffer = [VoucherProvider, VoucherOffer];

// -----------------------------------------------------------------------------
// Dashboard
// -----------------------------------------------------------------------------

export type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
  cfg?: UserConfig;
};

export const Dashboard = ({
  state: stateRaw,
  act,
  cfg,
}: DashboardProps): ReactElement => {
  const state = useMemo<DefaultView>(
    () => (defaultView as any)(stateRaw) as DefaultView,
    [stateRaw]
  );

  // Theme
  const [theme] = useContext(ThemeContext);

  // Overlay
  const [[overlayType, isOpen], setOverlay] = useState<[Overlay, boolean]>([
    'SEND',
    false,
  ]);

  const toggleOverlay = (type: Overlay) => {
    if (isOpen && overlayType !== type) {
      setOverlay([type, true]);
    } else if (isOpen && overlayType === type) {
      setOverlay([type, false]);
    } else if (!isOpen && overlayType !== type) {
      setOverlay([type, true]);
    } else if (!isOpen && overlayType === type) {
      setOverlay([type, true]);
    }
  };

  // User Interaction
  // Set transfer target, when clicking on contact action
  const [overwriteTo, setOverwriteTo] = useState<Address | undefined>();

  // Voucher Shop
  const [selectedOffer, setSelectedOffer] = useState<SelectedOffer>();
  const [justBoughtVoucher, setJustBoughtVoucher] = useState(false);

  const initializeVoucherOrder = (offer: SelectedOffer) => {
    setSelectedOffer(offer);
    toggleOverlay('CONFIRM_SEND');
  };

  // animation
  const getDelay = getIncrementor(0, 0.05);

  // -----------------------------------------------------------------------------
  // Data polling - Initial fetching and idle polling
  // -----------------------------------------------------------------------------

  const BALANCE_INTERVAL = 10000;
  const TRUST_NETWORK_INTERVAL = 15000;
  const UBI_PAYOUT_INTERVAL = 60000;
  const VOUCHER_INTERVAL = 30000;

  useEffect(() => {
    // Gather initial client information
    act(A._dashboard(A._getBalance(unit)));
    act(A._dashboard(A._getTrusts(unit)));
    act(A._dashboard(A._getUBIPayout(unit)));
    act(A._dashboard(A._getVouchers(getTimestamp())));
    act(A._dashboard(A._getVoucherProviders(unit)));

    // Start polling tasks
    const balancePolling = setInterval(() => {
      act(A._dashboard(A._getBalance(unit)));
    }, BALANCE_INTERVAL);

    const trustNetworkPolling = setInterval(() => {
      act(A._dashboard(A._getTrusts(unit)));
    }, TRUST_NETWORK_INTERVAL);

    const ubiPayoutPolling = setInterval(() => {
      act(A._dashboard(A._getTrusts(unit)));
    }, UBI_PAYOUT_INTERVAL);

    const voucherPolling = setInterval(() => {
      act(A._dashboard(A._getVouchers(getTimestamp())));
    }, VOUCHER_INTERVAL);

    const voucherProviderPolling = setInterval(() => {
      act(A._dashboard(A._getVoucherProviders(unit)));
    }, VOUCHER_INTERVAL);

    // Clear interval on unmount
    return () => {
      clearInterval(balancePolling);
      clearInterval(trustNetworkPolling);
      clearInterval(ubiPayoutPolling);
      clearInterval(voucherPolling);
      clearInterval(voucherProviderPolling);
    };
  }, []);

  // Increased polling after voucher buy
  useEffect(() => {
    let balancePolling: any, voucherPolling: any;

    if (justBoughtVoucher) {
      // console.log('Initialize faster polling..');
      balancePolling = setInterval(() => {
        act(A._dashboard(A._getBalance(unit)));
      }, 1500);

      voucherPolling = setInterval(() => {
        act(A._dashboard(A._getVouchers(getTimestamp())));
      }, 3000);
    } else if (!justBoughtVoucher) {
      // console.log('Finish faster polling');
      clearInterval(balancePolling);
      clearInterval(voucherPolling);
    }

    return () => {
      clearInterval(balancePolling);
      clearInterval(voucherPolling);
    };
  }, [justBoughtVoucher]);

  // -----------------------------------------------------------------------------
  // Data polling - Balance
  // -----------------------------------------------------------------------------

  const MAX_RETRYS = 10;
  const RETRY_INTERVAL = 1000;

  // Balance refresh after transfer

  const [initBalTransfer, setInitBalTransfer] = useState<string | null>(null);
  const [countRefreshTransfer, setCountRefreshTransfer] = useState<number>(0);

  useEffect(() => {
    switch (state.transferResult.type) {
      case 'loading':
        setCountRefreshTransfer(0);
        setInitBalTransfer(
          state.getBalanceResult.type === 'success'
            ? state.getBalanceResult.value.data.toString()
            : null
        );
        break;
      case 'success':
        const newBalance =
          state.getBalanceResult.type === 'success'
            ? state.getBalanceResult.value.data.toString()
            : null;

        if (
          newBalance === initBalTransfer &&
          countRefreshTransfer < MAX_RETRYS
        ) {
          setTimeout(() => {
            act(A._dashboard(A._getBalance(unit)));
            setCountRefreshTransfer(countRefreshTransfer + 1);
          }, RETRY_INTERVAL);
        }
        break;
      default:
        break;
    }
  }, [state.transferResult, countRefreshTransfer]);

  // Balance refresh after UBI payout

  const [initBalPayout, setInitBalPayout] = useState<string | null>(null);
  const [countRefreshPayout, setCountRefreshPayout] = useState<number>(0);

  useEffect(() => {
    switch (state.requestUBIPayoutResult.type) {
      case 'loading':
        setCountRefreshPayout(0);
        setInitBalPayout(
          state.getBalanceResult.type === 'success'
            ? state.getBalanceResult.value.data.toString()
            : null
        );
        break;
      case 'success':
        const newBalance =
          state.getBalanceResult.type === 'success'
            ? state.getBalanceResult.value.data.toString()
            : null;

        if (newBalance === initBalPayout && countRefreshPayout < MAX_RETRYS) {
          setTimeout(() => {
            act(A._dashboard(A._getBalance(unit)));
            setCountRefreshPayout(countRefreshPayout + 1);
          }, RETRY_INTERVAL);
        }
        break;
      default:
        break;
    }
  }, [state.requestUBIPayoutResult, countRefreshPayout]);

  // Balance for vouchers
  const [userBalance, setUserBalance] = useState<number>(0);

  useEffect(() => {
    if (state.getBalanceResult.type === 'success') {
      setUserBalance(
        parseFloat(
          displayBalance(
            state.getBalanceResult.value.data.toString(),
            'TIME-CIRCLES'
          )
        )
      );
    }
  }, [state.getBalanceResult]);

  // -----------------------------------------------------------------------------
  // Redeploy, if token is not deployed
  // -----------------------------------------------------------------------------

  useEffect(() => {
    if (
      state.checkUBIPayoutResult.type === 'failure' &&
      state.checkUBIPayoutResult.value.error.type === 'errNative' &&
      state.checkUBIPayoutResult.value.error.value.name === 'CoreError' &&
      state.checkUBIPayoutResult.value.error.value.message ===
        'Invalid Token address. Did you forget to deploy the Token?'
    ) {
      act(A._dashboard(A._redeploySafeAndToken(unit)));
    }
  }, [state.checkUBIPayoutResult]);

  // -----------------------------------------------------------------------------
  // User Search
  // -----------------------------------------------------------------------------

  const [search, setSearch] = useState<string>(''); // Search Input
  const [searchResult, setSearchResult] = useState<Trust[]>(state.usersSearch);

  useEffect(() => {
    // Query on user input
    if (search !== '') {
      act(A._dashboard(A._userSearch({ query: search })));
    } else {
      setSearchResult([]);
    }
  }, [search]);

  useEffect(() => {
    // Update on api result
    if (search !== '') {
      setSearchResult(state.usersSearch);
    } else {
      setSearchResult([]);
    }
  }, [state.usersSearch]);

  // -----------------------------------------------------------------------------
  // Transfer
  // -----------------------------------------------------------------------------

  useEffect(() => {
    if (state.transferResult.type === 'success') {
      // Close overlay
      setOverlay(['SEND', false]);
    }
  }, [state.transferResult]);

  // -----------------------------------------------------------------------------
  // Trust
  // -----------------------------------------------------------------------------

  return (
    <UserDashboard
      header={
        <HeaderContent>
          {/* <FadeIn orientation={'down'} delay={getDelay()}>
            <Icon path={mdiCog} size={1} color={theme.darkColor} />
          </FadeIn> */}
          <FadeIn orientation={'down'} delay={getDelay()}>
            <UserHandle>{`@${stateRaw.user.username}`}</UserHandle>
          </FadeIn>
          {/* <FadeIn orientation={'down'} delay={getDelay()}>
            <Icon path={mdiLogout} size={1} color={theme.darkColor} />
          </FadeIn> */}
        </HeaderContent>
      }
      text={
        <Text>
          <FadeIn orientation={'up'} delay={getDelay()}>
            <Balance
              theme={theme}
              balance={state.getBalanceResult}
              checkUBIPayoutResult={state.checkUBIPayoutResult}
              requestUBIPayoutResult={state.requestUBIPayoutResult}
            />
          </FadeIn>

          {/* <FadeIn orientation={orientation} delay={getDelay()}>
            <TrustGraph graph={graph} />
          </FadeIn> */}
        </Text>
      }
      control={
        <ControlContent>
          <FadeIn orientation={'up'} delay={getDelay()}>
            <TwoButtonRow>
              <Button
                prio="high"
                theme={theme}
                icon={mdiCashFast}
                onClick={() => toggleOverlay('SEND')}
              >
                {t('dashboard.sendButton')}
              </Button>
              <Button
                theme={theme}
                icon={mdiHandCoin}
                onClick={() => toggleOverlay('RECEIVE')}
              >
                {t('dashboard.receiveButton')}
              </Button>
            </TwoButtonRow>
          </FadeIn>
        </ControlContent>
      }
      mainContent={
        <>
          <MainContent>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <TrustUserList
                title={t('dashboard.trustNetworkTitle')}
                graph={state.graph}
                ownAddress={stateRaw.user.safeAddress}
                theme={theme}
                icon={mdiLan}
                toggleOverlay={toggleOverlay}
                setOverwriteTo={setOverwriteTo}
                addTrust={to => act(A._dashboard(A._addTrustConnection(to)))}
                trustAddResult={state.trustAddResult}
                removeTrust={to =>
                  act(A._dashboard(A._removeTrustConnection(to)))
                }
                trustRemoveResult={state.trustRemoveResult}
              />
            </FadeIn>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <UserSearch
                title={t('dashboard.exploreTitle')}
                trusts={searchResult}
                ownSafeAddress={stateRaw.user.safeAddress}
                theme={theme}
                icon={mdiMagnify}
                toggleOverlay={toggleOverlay}
                setOverwriteTo={setOverwriteTo}
                addTrust={to => act(A._dashboard(A._addTrustConnection(to)))}
                removeTrust={to =>
                  act(A._dashboard(A._removeTrustConnection(to)))
                }
                actionRow={
                  <JustifyBetweenCenter>
                    <Input
                      type="text"
                      value={search}
                      placeholder={t('dashboard.userSearchPlaceholder')}
                      onChange={e => setSearch(e.target.value)}
                    />
                  </JustifyBetweenCenter>
                }
              />
            </FadeIn>
          </MainContent>

          {cfg?.voucherShopEnabled && (
            <FadeIn orientation={'up'} delay={getDelay()}>
              <TopMargin>
                <LightColorFrame
                  theme={theme}
                  title={t('dashboard.voucherShop.shopTitle')}
                  icon={mdiGiftOutline}
                >
                  <MarginY size={2}>
                    <BuyVouchers
                      theme={theme}
                      providers={stateRaw.voucherProvidersResult}
                      initializeVoucherOrder={initializeVoucherOrder}
                      availableBalance={userBalance}
                    />
                  </MarginY>
                  <ListVouchers
                    theme={theme}
                    providersResult={stateRaw.voucherProvidersResult}
                    vouchersResult={state.vouchersResult}
                    justBoughtVoucher={justBoughtVoucher}
                    setJustBoughtVoucher={setJustBoughtVoucher}
                  />
                </LightColorFrame>
              </TopMargin>
            </FadeIn>
          )}

          <FadeIn orientation={'up'} delay={getDelay()}>
            <TopMargin>
              <LightColorFrame
                theme={theme}
                title="Trust Graph"
                icon={mdiGraphOutline}
              >
                <TrustGraph
                  graph={state.graph}
                  expandTrustNetwork={(addr: string) =>
                    act(A._dashboard(A._expandTrustNetwork(addr)))
                  }
                  theme={theme}
                />
              </LightColorFrame>
            </TopMargin>
          </FadeIn>
        </>
      }
      overlay={
        isOpen ? (
          <Overlay
            theme={theme}
            closeOverlay={() => setOverlay(['SEND', false])}
            content={
              <DashboardOverlay
                overlay={overlayType}
                closeOverlay={() => setOverlay(['SEND', false])}
                overwriteTo={overwriteTo}
                selectedOffer={selectedOffer}
                setJustBoughtVoucher={setJustBoughtVoucher}
                state={stateRaw}
                act={act}
                theme={theme}
              />
            }
          />
        ) : null
      }
      debug={
        <>
          <StateMachineDebugger state={state} />
          <Button
            theme={theme}
            onClick={() => act(A._dashboard(A._redeploySafeAndToken(unit)))}
          >
            Redeploy!
          </Button>
        </>
      }
    />
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const getTimestamp = () => Math.round(new Date().getTime() / 1000).toString();

// -----------------------------------------------------------------------------
// UI / DashboardOverlay
// -----------------------------------------------------------------------------

type DashboardOverlayProps = SendProps & {
  overlay: Overlay;
  closeOverlay: () => void;
  selectedOffer?: SelectedOffer;
  setJustBoughtVoucher: React.Dispatch<SetStateAction<boolean>>;
};

const DashboardOverlay = ({
  overlay,
  state,
  act,
  theme,
  closeOverlay,
  setJustBoughtVoucher,
  overwriteTo,
  selectedOffer,
}: DashboardOverlayProps): ReactElement | null => {
  switch (overlay) {
    case 'SEND':
      return (
        <Send overwriteTo={overwriteTo} state={state} act={act} theme={theme} />
      );
    case 'RECEIVE':
      return <Receive state={state} act={act} theme={theme} />;
    case 'CONFIRM_SEND':
      return selectedOffer ? (
        <ConfirmSend
          selectedOffer={selectedOffer}
          state={state}
          act={act}
          theme={theme}
          setJustBoughtVoucher={setJustBoughtVoucher}
        />
      ) : null;
  }
};

// -----------------------------------------------------------------------------
// UI / UserHandle
// -----------------------------------------------------------------------------

type UserHandleProps = { color?: string };

const UserHandle = styled.h2<UserHandleProps>(({ color }) => [
  tw`flex justify-around text-lg`,
  css`
    margin: 0;
    padding: 0;
    color: ${color || 'black'};
  `,
]);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const HeaderContent = tw.div`flex justify-around items-center mx-4`;
const ControlContent = tw.div`lg:my-2 md:my-4`;
const MainContent = tw.div`grid lg:grid-cols-2 gap-4`;
const ButtonText = tw.span`mr-3`;
const DebugButtonWrapper = tw.span`mb-3`;
const InputWrapper = tw.div`pr-2 w-4/5`;
const FlexBox = tw.div`flex flex-wrap lg:flex-row flex-col justify-between mb-4 gap-4 mx-2`;
const TopMargin = tw.div`mt-4`;

const FlexItemGrow = styled.div(() => [
  tw`h-full`,
  css`
    flex-grow: 1;
    flex-basis: 0;
  `,
]);
