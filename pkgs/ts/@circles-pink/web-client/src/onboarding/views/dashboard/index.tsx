import React, { ReactElement, useContext, useEffect, useState } from 'react';
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
import {
  mdiCashFast,
  mdiGiftOutline,
  mdiGraphOutline,
  mdiHandCoin,
  mdiLan,
  mdiMagnify,
} from '@mdi/js';
import { TrustUserList } from '../../../components/TrustUserList';
import {
  JustifyBetweenCenter,
  JustifyStartCenter,
  Margin,
  TwoButtonRow,
} from '../../../components/helper';
import { Balance } from './Balance';
import { StateMachineDebugger } from '../../../components/StateMachineDebugger';
import { TrustGraph } from '../../../components/TrustGraph/index';
import { UserSearch } from '../../../components/UserSearch';
import { ListVouchers } from './ListVouchers';
import { BuyVouchers } from './BuyVouchers';
import { LightColorFrame } from '../../../components/layout';
import { Frame } from '../../../components/Frame';
import { UserConfig } from '../../../types/user-config';
import { pipe } from 'fp-ts/lib/function';
import { useClientPolling } from './hooks/useClientPolling';
import {
  useUBIPayoutBalancePolling,
  useVoucherBalancePolling,
} from './hooks/useExtraBalancePolling';
import { useBoughtVouchersAmount } from './hooks/useBoughtVouchersAmount';
import { useBalance } from './hooks/useBalance';
import { useHealthCheck } from './hooks/useHealthCheck';
import { ControlContent, HeaderContent, MainContent, UserHandle } from './ui';
import { DashboardOverlay, OverlayTag } from './DashboardOverlay';
import { Overlay } from '../../../components/Overlay';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

export type MappedTrustNodes = Array<TrustNode & User>;

export type UserData = {
  username: string;
  avatarUrl: string | null;
};

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
  // -----------------------------------------------------------------------------
  // Data polling - Initial fetching and idle polling
  // -----------------------------------------------------------------------------

  // Perform an initial fetch and continuous polling of relevant dashboard data
  useClientPolling(act);

  // -----------------------------------------------------------------------------
  // Data polling - Balance polling on user action
  // -----------------------------------------------------------------------------

  // Poll Balance faster until voucher arrives
  useVoucherBalancePolling(act, state.vouchersResult, state.transferResult);

  // Poll Balance faster until UBI payout arrives
  useUBIPayoutBalancePolling(
    act,
    state.getBalanceResult,
    state.requestUBIPayoutResult
  );

  // -----------------------------------------------------------------------------
  // Utilities
  // -----------------------------------------------------------------------------

  // Theme
  const [theme] = useContext(ThemeContext);

  // Animation
  const getDelay = getIncrementor(0, 0.05);

  // Overlay
  const [[overlayType, isOpen], setOverlay] = useState<[OverlayTag, boolean]>([
    'SEND',
    false,
  ]);

  const toggleOverlay = (type: OverlayTag) => {
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

  // Close after successful transfer
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
  // User Interaction
  // -----------------------------------------------------------------------------

  // Set transfer target, when clicking on contact action
  const [overwriteTo, setOverwriteTo] = useState<Address | undefined>();

  // Voucher Shop
  const [selectedOffer, setSelectedOffer] = useState<SelectedOffer>();
  const [justBoughtVoucher, setJustBoughtVoucher] = useState(false);
  const boughtVouchersAmount = useBoughtVouchersAmount(state.vouchersResult);

  const initializeVoucherOrder = (offer: SelectedOffer) => {
    setSelectedOffer(offer);
    toggleOverlay('CONFIRM_SEND');
  };

  // Balance for vouchers
  const userBalance = useBalance(state.getBalanceResult);

  // -----------------------------------------------------------------------------
  // User Search
  // -----------------------------------------------------------------------------

  const [search, setSearch] = useState<string>(''); // Search Input

  useEffect(() => {
    act(_dashboardAction._userSearch({ query: search }));
  }, [search]);

  // -----------------------------------------------------------------------------
  // Redeploy, if token is not deployed
  // -----------------------------------------------------------------------------

  // Rarely the token is not deployed after account finalization
  // Perform a check and redeploy if necessary
  useHealthCheck(act, state.checkUBIPayoutResult);

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
                trustAddResult={state.trustAddResult}
                removeTrust={to =>
                  act(_dashboardAction._removeTrustConnection(to))
                }
                trustRemoveResult={state.trustRemoveResult}
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
                trustAddResult={state.trustAddResult}
                removeTrust={to =>
                  act(_dashboardAction._removeTrustConnection(to))
                }
                trustRemoveResult={state.trustRemoveResult}
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
              <Margin top={1}>
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
              </Margin>
            </FadeIn>
          )}

          <FadeIn orientation={'up'} delay={getDelay()}>
            <Margin top={1}>
              <LightColorFrame
                theme={theme}
                title="Trust Graph"
                icon={mdiGraphOutline}
              >
                <TrustGraph
                  graph={state.trusts}
                  expandTrustNetwork={(addr) =>
                    act(_dashboardAction._expandTrustNetwork(addr))
                  }
                  theme={theme}
                />
              </LightColorFrame>
            </Margin>
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
