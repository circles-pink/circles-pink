import React, { ReactElement, useContext, useEffect, useState } from 'react';
import {
  Button,
  ButtonLinkLike,
  Input,
  TagButton,
} from '../../../components/forms';
import { JustText, SubClaim, Text } from '../../../components/text';
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
  mdiShareVariantOutline,
  mdiWalletOutline,
} from '@mdi/js';
import { TrustUserList } from '../../../components/TrustUserList';
import { Overlay } from '../../../components/Overlay';
import {
  JustifyStartCenter,
  Margin,
  TwoButtonCol,
} from '../../../components/helper';
import { Balance } from './Balance';
import { StateMachineDebugger } from '../../../components/StateMachineDebugger';
import { TrustGraph } from '../../../components/TrustGraph/index';
import { UserSearch } from '../../../components/UserSearch';
import { ListVouchers } from './ListVouchers';
import { BuyVouchers } from './BuyVouchers';
import { LightColorFrame } from '../../../components/layout';
import { UserConfig } from '../../../types/user-config';
import { pipe } from 'fp-ts/lib/function';
import { XbgeUserDashboard } from '../../../components/XbgeUserDashboard';
import { DashboardOverlay, OverlayTag } from './DashboardOverlay';
import { MainContent, TwoCols } from './ui';
import { useHealthCheck } from './hooks/useHealthCheck';
import { useBalance } from './hooks/useBalance';
import { useBoughtVouchersAmount } from './hooks/useBoughtVouchersAmount';
import {
  useUBIPayoutBalancePolling,
  useVoucherBalancePolling,
} from './hooks/useExtraBalancePolling';
import { useClientPolling } from './hooks/useClientPolling';

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

export type XbgeDashboardProps = {
  state: DashboardState;
  act: (ac: DashboardAction) => void;
  cfg?: UserConfig;
  sharingFeature: ReactElement | null;
  buyVoucherEurLimit: number;
  shadowFriends?: Array<string>;
  xbgeSafeAddress?: string;
};

export const XbgeDashboard = ({
  state,
  act,
  cfg,
  sharingFeature,
  buyVoucherEurLimit,
  shadowFriends,
  xbgeSafeAddress,
}: XbgeDashboardProps): ReactElement => {
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
                            state.user.username
                          )}
                        </SubClaim>
                        <JustText fontSize={1.25}>
                          {t('dashboard.xbgeSpecial.yourBalance')}
                        </JustText>
                        <Balance
                          theme={theme}
                          balanceResult={state.getBalanceResult}
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

          {sharingFeature && (
            <Margin top={1}>
              <LightColorFrame
                title={t('dashboard.xbgeSpecial.shareFeatureTitle')}
                theme={theme}
                icon={mdiShareVariantOutline}
              >
                {sharingFeature}
              </LightColorFrame>
            </Margin>
          )}

          <FadeIn orientation={'up'} delay={getDelay()}>
            <Margin top={1}>
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
                  graph={state.trusts}
                  expandTrustNetwork={(addr: string) =>
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
            onClick={() => act(_dashboardAction._redeploySafeAndToken(unit))}
          >
            Redeploy!
          </Button>
        </>
      }
    />
  );
};
