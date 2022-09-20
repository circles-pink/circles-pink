import React, {
  ReactElement,
  SetStateAction,
  useContext,
  useEffect,
  useMemo,
  useState,
} from 'react';
import { Button, ButtonLinkLike, Input } from '../../../components/forms';
import { JustText, Text } from '../../../components/text';
import { UserDashboard } from '../../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import {
  Address,
  DashboardAction,
  DashboardState,
  TrustNode,
  unit,
  User,
  VoucherOffer,
  VoucherProvider,
  _Address,
  _RemoteData,
  _StateMachine,
  _VoucherServer,
} from '@circles-pink/state-machine/src';
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
  JustifyStartCenter,
  Margin,
  TwoButtonRow,
} from '../../../components/helper';
import { Send, SendProps } from './Send';
import { Receive } from './Receive';
import { Balance } from './Balance';
import { StateMachineDebugger } from '../../../components/StateMachineDebugger';
import { TrustGraph } from '../../../components/TrustGraph/index';
import { UserSearch } from '../../../components/UserSearch';
import { ListVouchers } from './ListVouchers';
import { BuyVouchers } from './BuyVouchers';
import { ConfirmSend } from './ConfirmSend';
import { displayBalance } from '../../utils/timeCircles';
import { LightColorFrame } from '../../../components/layout';
import { Frame } from '../../../components/Frame';
import { UserConfig } from '../../../types/user-config';
import { pipe } from 'fp-ts/lib/function';

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

const { _dashboardAction } = _StateMachine;

export type DashboardProps = {
  state: DashboardState;
  act: (ac: DashboardAction) => void;
  cfg?: UserConfig;
  buyVoucherEurLimit: number;
};

export const Dashboard = ({
  state,
  act,
  cfg,
  buyVoucherEurLimit,
}: DashboardProps): ReactElement => {
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
  const [boughtVouchersAmount, setBoughtVouchersAmount] = useState(0);

  const initializeVoucherOrder = (offer: SelectedOffer) => {
    setSelectedOffer(offer);
    toggleOverlay('CONFIRM_SEND');
  };

  useEffect(() => {
    pipe(
      state.vouchersResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {},
        onFailure: () => {},
        onSuccess: x => {
          setBoughtVouchersAmount(
            x.data.reduce(
              (p, c) => p + _VoucherServer.unVoucherAmount(c.amount),
              0
            )
          );
        },
      })
    );
  }, [state.vouchersResult]);

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
    act(_dashboardAction._getBalance(unit));
    act(_dashboardAction._getTrusts(unit));
    act(_dashboardAction._getUBIPayout(unit));
    act(_dashboardAction._getVouchers(getTimestamp()));
    act(_dashboardAction._getVoucherProviders(unit));

    // Start polling tasks
    const balancePolling = setInterval(() => {
      act(_dashboardAction._getBalance(unit));
    }, BALANCE_INTERVAL);

    const trustNetworkPolling = setInterval(() => {
      act(_dashboardAction._getTrusts(unit));
    }, TRUST_NETWORK_INTERVAL);

    const ubiPayoutPolling = setInterval(() => {
      act(_dashboardAction._getTrusts(unit));
    }, UBI_PAYOUT_INTERVAL);

    const voucherPolling = setInterval(() => {
      act(_dashboardAction._getVouchers(getTimestamp()));
    }, VOUCHER_INTERVAL);

    const voucherProviderPolling = setInterval(() => {
      act(_dashboardAction._getVoucherProviders(unit));
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
        act(_dashboardAction._getBalance(unit));
      }, 1500);

      voucherPolling = setInterval(() => {
        act(_dashboardAction._getVouchers(getTimestamp()));
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
    const balance = pipe(
      state.vouchersResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => null,
        onLoading: () => null,
        onFailure: () => null,
        onSuccess: x =>
          x.data
            .reduce((p, c) => p + _VoucherServer.unVoucherAmount(c.amount), 0)
            .toString(),
      })
    );

    pipe(
      state.transferResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {
          setCountRefreshTransfer(0);
          setInitBalTransfer(balance);
        },
        onFailure: () => {},
        onSuccess: () => {
          if (
            balance === initBalTransfer &&
            countRefreshTransfer < MAX_RETRYS
          ) {
            setTimeout(() => {
              act(_dashboardAction._getBalance(unit));
              setCountRefreshTransfer(countRefreshTransfer + 1);
            }, RETRY_INTERVAL);
          }
        },
      })
    );
  }, [state.transferResult, countRefreshTransfer]);

  // Balance refresh after UBI payout

  const [initBalPayout, setInitBalPayout] = useState<string | null>(null);
  const [countRefreshPayout, setCountRefreshPayout] = useState<number>(0);

  useEffect(() => {
    const balance = pipe(
      state.vouchersResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => null,
        onLoading: () => null,
        onFailure: () => null,
        onSuccess: x => x.data.toString(),
      })
    );

    pipe(
      state.requestUBIPayoutResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {
          setCountRefreshPayout(0);
          setInitBalPayout(balance);
        },
        onFailure: () => {},
        onSuccess: () => {
          if (balance === initBalPayout && countRefreshPayout < MAX_RETRYS) {
            setTimeout(() => {
              act(_dashboardAction._getBalance(unit));
              setCountRefreshPayout(countRefreshPayout + 1);
            }, RETRY_INTERVAL);
          }
        },
      })
    );
  }, [state.requestUBIPayoutResult, countRefreshPayout]);

  // Balance for vouchers
  const [userBalance, setUserBalance] = useState<number>(0);

  useEffect(() => {
    pipe(
      state.getBalanceResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {},
        onFailure: () => {},
        onSuccess: x => {
          setUserBalance(parseFloat(displayBalance(x.data, 'TIME-CIRCLES')));
        },
      })
    );
  }, [state.getBalanceResult]);

  // -----------------------------------------------------------------------------
  // Redeploy, if token is not deployed
  // -----------------------------------------------------------------------------

  useEffect(() => {
    pipe(
      state.checkUBIPayoutResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {},
        onFailure: x => {
          if (
            x.error.type === 'errNative' &&
            x.error.value.name === 'CoreError' &&
            x.error.value.message ===
              'Invalid Token address. Did you forget to deploy the Token?'
          ) {
            act(_dashboardAction._redeploySafeAndToken(unit));
          }
        },
        onSuccess: () => {},
      })
    );
  }, [state.checkUBIPayoutResult]);

  // -----------------------------------------------------------------------------
  // User Search
  // -----------------------------------------------------------------------------

  const [search, setSearch] = useState<string>(''); // Search Input

  // -----------------------------------------------------------------------------
  // Transfer
  // -----------------------------------------------------------------------------

  useEffect(() => {
    pipe(
      state.transferResult,

      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {},
        onFailure: () => {},
        onSuccess: () => {
          // Close overlay
          setOverlay(['SEND', false]);
        },
      })
    );
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
            <UserHandle>{`@${state.user.username}`}</UserHandle>
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
              balanceResult={state.getBalanceResult}
              requestUBIPayoutResult={state.requestUBIPayoutResult}
            />
          </FadeIn>
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
                address={_Address.Address(state.user.safeAddress)}
                title={t('dashboard.trustNetworkTitle')}
                graph={state.trusts}
                theme={theme}
                icon={mdiLan}
                toggleOverlay={toggleOverlay}
                setOverwriteTo={setOverwriteTo}
                addTrust={to => act(_dashboardAction._addTrustConnection(to))}
                // trustAddResult={state.trustAddResult}
                removeTrust={to =>
                  act(_dashboardAction._removeTrustConnection(to))
                }
                // trustRemoveResult={state.trustRemoveResult}
              />
            </FadeIn>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <UserSearch
                userSearchResult={state.userSearchResult}
                trusts={state.trusts}
                onSearch={query => act(_dashboardAction._userSearch({ query }))}
                onAddTrust={userIdent =>
                  act(_dashboardAction._addTrustConnection(userIdent))
                }
                centerAddress={_Address.Address(state.user.safeAddress)}
                title={t('dashboard.exploreTitle')}
                theme={theme}
                icon={mdiMagnify}
                toggleOverlay={toggleOverlay}
                setOverwriteTo={setOverwriteTo}
                addTrust={to => act(_dashboardAction._addTrustConnection(to))}
                removeTrust={to =>
                  act(_dashboardAction._removeTrustConnection(to))
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
                <Frame
                  theme={theme}
                  title={t('dashboard.voucherShop.shopTitle')}
                  icon={mdiGiftOutline}
                >
                  <Margin top={2} bottom={2}>
                    <BuyVouchers
                      theme={theme}
                      providers={state.voucherProvidersResult}
                      initializeVoucherOrder={initializeVoucherOrder}
                      availableBalance={userBalance}
                      boughtVouchersAmount={boughtVouchersAmount}
                      buyVoucherEurLimit={buyVoucherEurLimit}
                    />
                  </Margin>
                  <ListVouchers
                    theme={theme}
                    providersResult={state.voucherProvidersResult}
                    vouchersResult={state.vouchersResult}
                    justBoughtVoucher={justBoughtVoucher}
                    setJustBoughtVoucher={setJustBoughtVoucher}
                  />

                  <Margin top={3}>
                    <JustifyStartCenter>
                      <JustText>
                        {t('dashboard.voucherShop.buyAtMarketPlace')}
                      </JustText>
                      <ButtonLinkLike
                        onClick={() =>
                          window.open(
                            'https://market.joincircles.net/',
                            '_blank'
                          )
                        }
                      >
                        {t('dashboard.voucherShop.toTheMarketPlace')}
                      </ButtonLinkLike>
                    </JustifyStartCenter>
                  </Margin>
                </Frame>
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
                  graph={state.trusts}
                  expandTrustNetwork={(addr: string) =>
                    act(_dashboardAction._expandTrustNetwork(addr))
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
                state={state}
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
            onClick={() => act(_dashboardAction._redeploySafeAndToken(unit))}
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

const UserHandle = styled.span<UserHandleProps>(({ color }) => [
  tw`flex justify-around text-lg`,
  css`
    margin: 0;
    padding: 0;
    font-weight: 600;
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
