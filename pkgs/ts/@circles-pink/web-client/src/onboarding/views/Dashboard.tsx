import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import React, { ReactElement, useContext, useEffect, useState } from 'react';
import { Button, Input } from '../../components/forms';
import { Text } from '../../components/text';
import { UserDashboard } from '../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import {
  DashboardState,
  _inSync,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import {
  DefaultView,
  defaultView,
  Trusts,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { mapResult } from '../utils/mapResult';
import tw, { css, styled } from 'twin.macro';
import {
  mdiCashFast,
  mdiCog,
  mdiHandCoin,
  mdiLan,
  mdiLogout,
  mdiMagnify,
} from '@mdi/js';
import { TrustUserList } from '../../components/TrustUserList';
import { Overlay } from '../../components/Overlay';
import { JustifyBetweenCenter, TwoButtonRow } from '../../components/helper';
import { Send, SendProps } from './dashboard/Send';
import { Receive } from './dashboard/Receive';
import { Balance } from './dashboard/Balance';
import {
  TrustNode,
  User,
} from '@circles-pink/state-machine/output/CirclesCore';
import ReactTooltip from 'react-tooltip';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import { paginate } from '../utils/paginate';

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
  const state = (defaultView as any)(stateRaw) as DefaultView;

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
  const [overwriteTo, setOverwriteTo] = useState<string>('');

  // animation
  const getDelay = getIncrementor(0, 0.05);

  // -----------------------------------------------------------------------------
  // Side Effects
  // -----------------------------------------------------------------------------

  useEffect(() => {
    // Gather initial Client information
    act(A._dashboard(A._getBalance(unit)));
    act(A._dashboard(A._getTrusts(unit)));
  }, []);

  // -----------------------------------------------------------------------------
  // User Search
  // -----------------------------------------------------------------------------

  // Search Input
  const [search, setSearch] = useState<string>('');

  // Mapped with trust data
  const [mappedSearch, setMappedSearch] = useState<Trusts>([]);

  useEffect(() => {
    // Query on user input
    if (search !== '') {
      act(A._dashboard(A._userSearch({ query: search })));
    }
  }, [search]);

  useEffect(() => {
    // Map received userdata with users trusts for display
    if (
      state.userSearchResult.type === 'success' &&
      state.trustsResult.type === 'success'
    ) {
      const trusts = state.trusts;
      const mapped = state.userSearchResult.value.map(u => {
        const t = trusts.find(t => t.safeAddress === u.safeAddress);
        return {
          ...u,
          isIncoming: t?.isIncoming || false,
          isOutgoing: t?.isOutgoing || false,
          trustState: t?.trustState || _inSync,
          user: {
            username: u.username,
            avatarUrl: u.avatarUrl,
            id: u.id,
            safeAddress: u.safeAddress,
          },
          // limitPercentageIn: t?.limitPercentageIn || 0,
          // limitPercentageOut: t?.limitPercentageOut || 0,
          // mutualConnections: t?.mutualConnections || [],
        };
      });
      setMappedSearch(mapped);
    }
  }, [state.userSearchResult, state.trustsResult]);

  // -----------------------------------------------------------------------------
  // Transfer
  // -----------------------------------------------------------------------------

  useEffect(() => {
    if (state.transferResult.type === 'success') {
      // Close overlay
      setOverlay(['SEND', false]);
      // Refresh balance - better be done in purs
      setTimeout(() => {
        act(A._dashboard(A._getBalance(unit)));
      }, 2000);
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
            <Balance theme={theme} balance={state.getBalanceResult} />
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
        <MainContent>
          <FadeIn orientation={'up'} delay={getDelay()}>
            <TrustUserList
              title={t('dashboard.trustNetworkTitle')}
              trusts={state.trusts}
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
            <TrustUserList
              title={t('dashboard.exploreTitle')}
              trusts={mappedSearch}
              theme={theme}
              icon={mdiMagnify}
              toggleOverlay={toggleOverlay}
              setOverwriteTo={setOverwriteTo}
              addTrust={to => act(A._dashboard(A._addTrustConnection(to)))}
              trustAddResult={state.trustAddResult}
              removeTrust={to =>
                act(A._dashboard(A._removeTrustConnection(to)))
              }
              trustRemoveResult={state.trustRemoveResult}
              actionRow={
                <JustifyBetweenCenter>
                  <Input
                    type="text"
                    value={search}
                    placeholder={'Search by username'}
                    onChange={e => setSearch(e.target.value)}
                  />
                  {/* <DebugButtonWrapper>
                      <Button
                        prio={'high'}
                        theme={theme}
                        state={mapResult(state.userSearchResult)}
                        onClick={() =>
                          act(A._dashboard(A._userSearch({ query: search })))
                        }
                      >
                        {t('dashboard.searchButton')}
                      </Button>
                    </DebugButtonWrapper> */}
                </JustifyBetweenCenter>
              }
            />
          </FadeIn>
        </MainContent>
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
      debug={<StateMachineDebugger state={state} />}
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

const FlexItemGrow = styled.div(() => [
  tw`h-full`,
  css`
    flex-grow: 1;
    flex-basis: 0;
  `,
]);
