import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import React, {
  ReactElement,
  useContext,
  useEffect,
  useMemo,
  useState,
} from 'react';
import { Button, Input } from '../../components/forms';
import { Text } from '../../components/text';
import { UserDashboard } from '../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import { DashboardState } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import {
  DefaultView,
  defaultView,
  Trust,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import tw, { css, styled } from 'twin.macro';
import {
  mdiCashFast,
  mdiCog,
  mdiGraph,
  mdiGraphOutline,
  mdiHandCoin,
  mdiLan,
  mdiLogout,
  mdiMagnify,
} from '@mdi/js';
import {
  /*LightColorFrame,*/ TrustUserList,
} from '../../components/TrustUserList';
import { Overlay } from '../../components/Overlay';
import { JustifyBetweenCenter, TwoButtonRow } from '../../components/helper';
import { Send, SendProps } from './dashboard/Send';
import { Receive } from './dashboard/Receive';
import { Balance } from './dashboard/Balance';
import {
  TrustNode,
  User,
} from '@circles-pink/state-machine/output/CirclesCore';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { TrustGraph } from '../../components/TrustGraph/index';
import { UserSearch } from '../../components/UserSearch';
import {
  isPendingTrust,
  isPendingUntrust,
} from '@circles-pink/state-machine/output/CirclesPink.Data.TrustState';
import { fromFpTsTuple, toFpTsTuple } from '../../utils/fpTs';
import { Frame } from '../../components/Frame';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

export type MappedTrustNodes = Array<TrustNode & User>;

export type UserData = {
  username: string;
  avatarUrl: string | null;
};

export type Overlay = 'SEND' | 'RECEIVE';

// -----------------------------------------------------------------------------
// Dashboard
// -----------------------------------------------------------------------------

export type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
};

export const Dashboard = ({
  state: stateRaw,
  act,
}: DashboardProps): ReactElement => {
  const state = useMemo<DefaultView>(
    () => (defaultView as any)(stateRaw) as DefaultView,
    [stateRaw]
  );

  const trusts = state.trustsConfirmed.concat(state.trustsCandidates);

  // Theme
  const [theme] = useContext(ThemeContext);

  // Overlay
  const [overlay, setOverlay] = useState<[Overlay, boolean]>(['SEND', false]);

  const toggleOverlay = (type: Overlay) => {
    if (overlay[1] && overlay[0] !== type) {
      setOverlay([type, true]);
    } else if (overlay[1] && overlay[0] === type) {
      setOverlay([type, false]);
    } else if (!overlay[1] && overlay[0] !== type) {
      setOverlay([type, true]);
    } else if (!overlay[1] && overlay[0] === type) {
      setOverlay([type, true]);
    }
  };

  // User Interaction
  // Set transfer target, when clicking on contact action
  const [overwriteTo, setOverwriteTo] = useState<Address | undefined>();

  // animation
  const getDelay = getIncrementor(0, 0.05);

  // -----------------------------------------------------------------------------
  // Data polling - Initial fetching and idle polling
  // -----------------------------------------------------------------------------

  const BALANCE_INTERVAL = 10000;
  const TRUST_NETWORK_INTERVAL = 15000;
  const UBI_PAYOUT_INTERVAL = 60000;

  useEffect(() => {
    // Gather initial client information
    act(A._dashboard(A._getBalance(unit)));
    act(A._dashboard(A._getTrusts(unit)));
    act(A._dashboard(A._getUBIPayout(unit)));

    // Start polling tasks
    const balancePolling = setInterval(() => {
      act(A._dashboard(A._getBalance(unit)));
    }, BALANCE_INTERVAL);

    const trustNetworkPolling = setInterval(() => {
      act(A._dashboard(A._getTrusts(unit)));
    }, TRUST_NETWORK_INTERVAL);

    const UBIPayoutPolling = setInterval(() => {
      act(A._dashboard(A._getTrusts(unit)));
    }, UBI_PAYOUT_INTERVAL);

    // Clear interval on unmount
    return () => {
      clearInterval(balancePolling);
      clearInterval(trustNetworkPolling);
      clearInterval(UBIPayoutPolling);
    };
  }, []);

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
          // console.log('Update:', countRefreshTransfer + 1);
          setTimeout(() => {
            act(A._dashboard(A._getBalance(unit)));
            setCountRefreshTransfer(countRefreshTransfer + 1);
          }, RETRY_INTERVAL);
        } else {
          // console.log('Finished after:', countRefreshTransfer);
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
          // console.log('Update:', countRefreshPayout + 1);
          setTimeout(() => {
            act(A._dashboard(A._getBalance(unit)));
            setCountRefreshPayout(countRefreshPayout + 1);
          }, RETRY_INTERVAL);
        } else {
          // console.log('Finished after:', countRefreshPayout);
        }
        break;
      default:
        break;
    }
  }, [state.requestUBIPayoutResult, countRefreshPayout]);

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

  // const graph: Graph = new Map([[state.user.safeAddress, state.trusts]]);

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
              <Frame theme={theme}>
                <TrustUserList
                // title={t('dashboard.trustNetworkTitle')}
                // graph={state.graph}
                // ownAddress={stateRaw.user.safeAddress}
                // theme={theme}
                // icon={mdiLan}
                // toggleOverlay={toggleOverlay}
                // setOverwriteTo={setOverwriteTo}
                // addTrust={to => act(A._dashboard(A._addTrustConnection(to)))}
                // trustAddResult={state.trustAddResult}
                // removeTrust={to =>
                //   act(A._dashboard(A._removeTrustConnection(to)))
                // }
                // trustRemoveResult={state.trustRemoveResult}
                />
              </Frame>
            </FadeIn>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <Frame theme={theme}>
                <UserSearch
                // title={t('dashboard.exploreTitle')}
                // trusts={searchResult}
                // theme={theme}
                // icon={mdiMagnify}
                // toggleOverlay={toggleOverlay}
                // setOverwriteTo={setOverwriteTo}
                // addTrust={to => act(A._dashboard(A._addTrustConnection(to)))}
                // removeTrust={to =>
                //   act(A._dashboard(A._removeTrustConnection(to)))
                // }
                // actionRow={
                //   <JustifyBetweenCenter>
                //     <Input
                //       type="text"
                //       value={search}
                //       placeholder={t('dashboard.userSearchPlaceholder')}
                //       onChange={e => setSearch(e.target.value)}
                //     />
                //   </JustifyBetweenCenter>
                // }
                />
              </Frame>
            </FadeIn>
          </MainContent>
          <FadeIn orientation={'up'} delay={getDelay()}>
            <TopMargin>
              <Frame theme={theme} title="Trust Graph" icon={mdiGraphOutline}>
                <TrustGraph
                  graph={state.graph}
                  expandTrustNetwork={(addr: string) =>
                    act(A._dashboard(A._expandTrustNetwork(addr)))
                  }
                  theme={theme}
                />
              </Frame>
            </TopMargin>
          </FadeIn>
        </>
      }
      overlay={
        overlay[1] ? (
          <Overlay
            theme={theme}
            closeOverlay={() => setOverlay(['SEND', false])}
            content={
              <DashboardOverlay
                overlay={overlay[0]}
                closeOverlay={() => setOverlay(['SEND', false])}
                overwriteTo={overwriteTo}
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
// UI / DashboardOverlay
// -----------------------------------------------------------------------------

type OverlayType = 'SEND' | 'RECEIVE';

type DashboardOverlayProps = SendProps & {
  overlay: OverlayType;
  closeOverlay: () => void;
};

const DashboardOverlay = ({
  overlay,
  state,
  act,
  theme,
  closeOverlay,
  overwriteTo,
}: DashboardOverlayProps) => {
  switch (overlay) {
    case 'SEND':
      return (
        <Send overwriteTo={overwriteTo} state={state} act={act} theme={theme} />
      );

    case 'RECEIVE':
      return <Receive state={state} act={act} theme={theme} />;
  }
};

// -----------------------------------------------------------------------------
// UI / UserHandle
// -----------------------------------------------------------------------------

type UserHandleProps = { color?: string };

const UserHandle = styled.h2<UserHandleProps>(({ color }) => [
  tw`flex justify-around text-lg`,
  css`
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
