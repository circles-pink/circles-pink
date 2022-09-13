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
import {
  Button,
  ButtonLinkLike,
  Input,
  TagButton,
} from '../../../components/forms';
import { JustText, SubClaim, Text } from '../../../components/text';
import { XbgeUserDashboard } from '../../../components/XbgeUserDashboard';
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
  mdiShareVariantOutline,
  mdiWalletOutline,
} from '@mdi/js';
import { TrustUserList } from '../../../components/TrustUserList';
import { Overlay } from '../../../components/Overlay';
import {
  JustifyBetweenCenter,
  JustifyStartCenter,
  Margin,
  TwoButtonCol,
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
import { LightColorFrame } from '../../../components/layout';
import { UserConfig } from '../../../types/user-config';

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

export type XbgeDashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
  cfg?: UserConfig;
  sharingFeature: ReactElement | null;
  buyVoucherEurLimit: number;
  shadowFriends?: Array<string>;
  xbgeSafeAddress?: string;
};

export const XbgeDashboard = ({
  state: stateRaw,
  act,
  cfg,
  sharingFeature,
  buyVoucherEurLimit,
  shadowFriends,
  xbgeSafeAddress,
}: XbgeDashboardProps): ReactElement => {
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
  const [boughtVouchersAmount, setBoughtVouchersAmount] = useState(0);

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

  useEffect(() => {
    if (state.vouchersResult.type === 'success') {
      setBoughtVouchersAmount(
        state.vouchersResult.value.data.reduce((p, c) => p + c.amount, 0)
      );
    }
  }, [state.vouchersResult]);

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
    <XbgeUserDashboard
      mainContent={
        <>
          <FadeIn orientation={'up'} delay={getDelay()}>
            <Margin bottom={1}>
              <LightColorFrame
                theme={theme}
                title={t('dashboard.xbgeSpecial.myWalletTitle')}
                icon={mdiWalletOutline}
              >
                <TwoCols>
                  <Text>
                    <FadeIn orientation={'up'} delay={getDelay()}>
                      <>
                        <SubClaim>
                          {t('dashboard.xbgeSpecial.welcomeGeneral')}
                        </SubClaim>
                        <SubClaim>
                          {t('dashboard.xbgeSpecial.welcomeUser').replace(
                            '{{user}}',
                            stateRaw.user.username
                          )}
                        </SubClaim>
                        <JustText fontSize={1.25}>
                          {t('dashboard.xbgeSpecial.yourBalance')}
                        </JustText>
                        <Balance
                          theme={theme}
                          balance={state.getBalanceResult}
                          checkUBIPayoutResult={state.checkUBIPayoutResult}
                          requestUBIPayoutResult={state.requestUBIPayoutResult}
                        />
                      </>
                    </FadeIn>
                  </Text>
                  <TwoButtonCol>
                    <Button
                      prio="high"
                      theme={theme}
                      icon={mdiCashFast}
                      onClick={() => toggleOverlay('SEND')}
                      fullWidth
                    >
                      {t('dashboard.xbgeSpecial.sendButton')}
                    </Button>
                    <Button
                      theme={theme}
                      icon={mdiHandCoin}
                      onClick={() => toggleOverlay('RECEIVE')}
                      fullWidth
                    >
                      {t('dashboard.xbgeSpecial.receiveButton')}
                    </Button>
                  </TwoButtonCol>
                </TwoCols>
              </LightColorFrame>
            </Margin>
          </FadeIn>

          <FadeIn orientation={'up'} delay={getDelay()}>
            <Margin bottom={1}>
              <LightColorFrame
                theme={theme}
                title={t('dashboard.voucherShop.shopTitle')}
                icon={mdiGiftOutline}
              >
                <>
                  <>
                    {t('dashboard.xbgeSpecial.whatToBuyForCircles') && (
                      <Margin top={1} bottom={1}>
                        <JustText fontSize={1.25}>
                          {
                            t(
                              'dashboard.xbgeSpecial.whatToBuyForCircles'
                            ).split('{{providers}}')[0]
                          }
                          <ButtonLinkLike
                            fontSize={1.25}
                            onClick={() =>
                              window.open('https://goodbuy.eu/', '_blank')
                            }
                          >
                            goodbuy.eu
                          </ButtonLinkLike>
                          ,{' '}
                          <ButtonLinkLike
                            fontSize={1.25}
                            onClick={() =>
                              window.open('https://sirplus.de/', '_blank')
                            }
                          >
                            sirplus.de
                          </ButtonLinkLike>{' '}
                          und{' '}
                          <ButtonLinkLike
                            fontSize={1.25}
                            onClick={() =>
                              window.open('https://geschmack.org/', '_blank')
                            }
                          >
                            geschmack.org
                          </ButtonLinkLike>
                          {
                            t(
                              'dashboard.xbgeSpecial.whatToBuyForCircles'
                            ).split('{{providers}}')[1]
                          }
                        </JustText>
                      </Margin>
                    )}
                  </>
                  {cfg?.voucherShopEnabled ? (
                    <>
                      <Margin top={2} bottom={2}>
                        <BuyVouchers
                          theme={theme}
                          providers={stateRaw.voucherProvidersResult}
                          initializeVoucherOrder={initializeVoucherOrder}
                          availableBalance={userBalance}
                          boughtVouchersAmount={boughtVouchersAmount}
                          buyVoucherEurLimit={buyVoucherEurLimit}
                        />
                      </Margin>
                      <ListVouchers
                        theme={theme}
                        providersResult={stateRaw.voucherProvidersResult}
                        vouchersResult={state.vouchersResult}
                        justBoughtVoucher={justBoughtVoucher}
                        setJustBoughtVoucher={setJustBoughtVoucher}
                      />
                    </>
                  ) : (
                    <>
                      <Margin top={1} bottom={1}>
                        <JustText fontSize={1.25}>
                          {
                            t(
                              'dashboard.xbgeSpecial.howToActivateVoucherShop'
                            ).split('{{collectionTipps}}')[0]
                          }
                          <ButtonLinkLike
                            fontSize={1.25}
                            onClick={() =>
                              window.open(
                                t('dashboard.xbgeSpecial.collectionTippsLink'),
                                '_blank'
                              )
                            }
                          >
                            {t('dashboard.xbgeSpecial.collectionTipps')}
                          </ButtonLinkLike>
                          {
                            t(
                              'dashboard.xbgeSpecial.howToActivateVoucherShop'
                            ).split('{{collectionTipps}}')[1]
                          }
                        </JustText>
                      </Margin>
                      <Margin top={1} bottom={1}>
                        <JustText fontSize={1.25}>
                          {t('dashboard.xbgeSpecial.bringSignaturesTo')}
                        </JustText>
                      </Margin>
                    </>
                  )}
                </>

                <Margin top={2}>
                  <JustifyStartCenter>
                    <JustText fontSize={1}>
                      {
                        t('dashboard.voucherShop.buyAtMarketPlace').split(
                          '{{toTheMarketPlace}}'
                        )[0]
                      }
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
                      {
                        t('dashboard.voucherShop.buyAtMarketPlace').split(
                          '{{toTheMarketPlace}}'
                        )[1]
                      }
                    </JustText>
                  </JustifyStartCenter>
                </Margin>
              </LightColorFrame>
            </Margin>
          </FadeIn>

          <MainContent>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <TrustUserList
                address={stateRaw.user.safeAddress}
                title={t('dashboard.trustNetworkTitle')}
                graph={stateRaw.trusts}
                theme={theme}
                icon={mdiLan}
                toggleOverlay={toggleOverlay}
                setOverwriteTo={setOverwriteTo}
                addTrust={to => act(A._dashboard(A._addTrustConnection(to)))}
                // trustAddResult={state.trustAddResult}
                removeTrust={to =>
                  act(A._dashboard(A._removeTrustConnection(to)))
                }
                // trustRemoveResult={state.trustRemoveResult}
              />
            </FadeIn>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <UserSearch
                userSearchResult={stateRaw.userSearchResult}
                trusts={stateRaw.trusts}
                onSearch={query => act(A._dashboard(A._userSearch({ query })))}
                onAddTrust={userIdent =>
                  act(A._dashboard(A._addTrustConnection(userIdent)))
                }
                centerAddress={stateRaw.user.safeAddress}
                title={t('dashboard.exploreTitle')}
                theme={theme}
                icon={mdiMagnify}
                toggleOverlay={toggleOverlay}
                setOverwriteTo={setOverwriteTo}
                addTrust={to => act(A._dashboard(A._addTrustConnection(to)))}
                removeTrust={to =>
                  act(A._dashboard(A._removeTrustConnection(to)))
                }
                description={t('dashboard.xbgeSpecial.userSearchDescription')}
                actionRow={
                  <div>
                    <Input
                      type="text"
                      value={search}
                      placeholder={t('dashboard.userSearchPlaceholder')}
                      onChange={e => setSearch(e.target.value)}
                    />
                    <>
                      {shadowFriends && shadowFriends.length > 0 && (
                        <>
                          <Margin bottom={0.5}>
                            <JustText fontSize={0.75}>
                              {t('dashboard.xbgeSpecial.yourShadowFriends')}
                            </JustText>
                          </Margin>
                          {shadowFriends.map((friend, index) => (
                            <TagButton
                              theme={theme}
                              onClick={() => setSearch(friend)}
                              key={`${friend}-${index}`}
                            >
                              {friend}
                            </TagButton>
                          ))}
                        </>
                      )}
                    </>
                  </div>
                }
              />
            </FadeIn>
          </MainContent>

          {/* <MainContent>
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
                description={t('dashboard.xbgeSpecial.trustNetworkDescription')}
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
                description={t('dashboard.xbgeSpecial.userSearchDescription')}
                actionRow={
                  <div>
                    <Input
                      type="text"
                      value={search}
                      placeholder={t('dashboard.userSearchPlaceholder')}
                      onChange={e => setSearch(e.target.value)}
                    />
                    <>
                      {shadowFriends && shadowFriends.length > 0 && (
                        <>
                          <Margin bottom={0.5}>
                            <JustText fontSize={0.75}>
                              {t('dashboard.xbgeSpecial.yourShadowFriends')}
                            </JustText>
                          </Margin>
                          {shadowFriends.map((friend, index) => (
                            <TagButton
                              theme={theme}
                              onClick={() => setSearch(friend)}
                              key={`${friend}-${index}`}
                            >
                              {friend}
                            </TagButton>
                          ))}
                        </>
                      )}
                    </>
                  </div>
                }
              />
            </FadeIn>
          </MainContent> */}

          {sharingFeature && (
            <TopMargin>
              <LightColorFrame
                title={t('dashboard.xbgeSpecial.shareFeatureTitle')}
                theme={theme}
                icon={mdiShareVariantOutline}
              >
                {sharingFeature}
              </LightColorFrame>
            </TopMargin>
          )}

          <FadeIn orientation={'up'} delay={getDelay()}>
            <TopMargin>
              <LightColorFrame
                theme={theme}
                title="Trust Graph"
                icon={mdiGraphOutline}
              >
                <>
                  {t('dashboard.xbgeSpecial.trustGraphDescription') && (
                    <Margin top={0.75} bottom={0.75}>
                      <JustText fontSize={1.25}>
                        {t('dashboard.xbgeSpecial.trustGraphDescription')}
                      </JustText>
                    </Margin>
                  )}
                </>

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
                xbgeSafeAddress={xbgeSafeAddress}
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
  xbgeSafeAddress?: string;
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
  xbgeSafeAddress,
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
          xbgeSafeAddress={xbgeSafeAddress}
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
const TwoCols = tw.div`max-w-7xl grid lg:grid-cols-2 md:grid-cols-2 lg:gap-4 md:gap-4`;

const FlexItemGrow = styled.div(() => [
  tw`h-full`,
  css`
    flex-grow: 1;
    flex-basis: 0;
  `,
]);
