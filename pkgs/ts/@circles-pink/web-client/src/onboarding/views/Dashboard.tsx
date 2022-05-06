import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import React, {
  ReactElement,
  SetStateAction,
  useContext,
  useEffect,
  useState,
} from 'react';
import { Button, Input } from '../../components/forms';
import { Text } from '../../components/text';
import { UserDashboard } from '../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import { DashboardState } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
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
import Icon from '@mdi/react';
import {
  MappedTrustNodes,
  TrustUserList,
} from '../../components/TrustUserList';
import {
  TrustNode,
  User,
} from '@circles-pink/state-machine/output/CirclesCore';
import { Overlay } from '../../components/Overlay';
import { JustifyBetweenCenter } from '../../components/helper';
import { Send, SendProps } from './dashboard/Send';
import { Receive } from './dashboard/Receive';
import { Balance } from './dashboard/Balance';

// -----------------------------------------------------------------------------
// Dashboard
// -----------------------------------------------------------------------------

export type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
};

export const Dashboard = ({ state, act }: DashboardProps): ReactElement => {
  // Theme
  const [theme] = useContext(ThemeContext);

  // Overlay
  type Overlay = 'SEND' | 'RECEIVE';
  const [overlayOpen, setOverlayOpen] = useState<boolean>(false);
  const [activeOverlay, setActiveOverlay] = useState<Overlay>('SEND');

  const toggleOverlay = (type: Overlay) => {
    if (!overlayOpen) {
      setActiveOverlay(type);
      setOverlayOpen(true);
    } else if (overlayOpen && activeOverlay !== type) {
      setActiveOverlay(type);
    } else {
      setOverlayOpen(!overlayOpen);
    }
  };

  // User Interaction
  // Set transfer target
  const [overwriteTo, setOverwriteTo] = useState<string>('');

  // Search
  const [search, setSearch] = useState<string>('');

  // animation
  const getDelay = getIncrementor(0, 0.05);

  // -----------------------------------------------------------------------------
  // Side Effects
  // -----------------------------------------------------------------------------

  useEffect(() => {
    // Gather initial Client information
    act(A._dashboard(A._getBalance(unit)));
    act(A._dashboard(A._getTrusts(unit))); // Should be done in control

    // Setup polling intervals
    const pollTrusts = window.setInterval(
      () => act(A._dashboard(A._getTrusts(unit))),
      15 * 1000
    );

    // Clear polling intervals
    return () => {
      window.clearInterval(pollTrusts);
    };
  }, []);

  // -----------------------------------------------------------------------------
  // User Search
  // -----------------------------------------------------------------------------

  const [mappedSearch, setMappedSearch] = useState<MappedTrustNodes>([]);

  useEffect(() => {
    // Map received userdata with users trusts for display
    if (state.userSearchResult.type === 'success') {
      const mapped = state.userSearchResult.value.map(u => {
        const trusts = state.trusts as TrustNode[];
        const t = trusts.find(t => t.safeAddress === u.safeAddress);
        return {
          ...u,
          isIncoming: t?.isIncoming || false,
          isOutgoing: t?.isOutgoing || false,
          limitPercentageIn: t?.limitPercentageIn || 0,
          limitPercentageOut: t?.limitPercentageOut || 0,
          mutualConnections: t?.mutualConnections || [],
        };
      });
      setMappedSearch(mapped);
    }
  }, [state.userSearchResult]);

  // -----------------------------------------------------------------------------
  // Trusts get userdata
  // -----------------------------------------------------------------------------

  const [mappedTrusts, setMappedTrusts] = useState<MappedTrustNodes>([]);

  useEffect(() => {
    // Whenever trusts are updated, we wanna get the according usernames
    const addresses = state.trusts.map(t => t.safeAddress);
    act(A._dashboard(A._getUsers({ userNames: [], addresses })));
  }, [state.trusts]);

  useEffect(() => {
    // Map received userdata with users trusts for display
    if (state.getUsersResult.type === 'success') {
      const mapped = state.trusts.map(t => {
        const users = state.getUsersResult.value as User[];
        const info = users.find(u => u.safeAddress === t.safeAddress);
        return {
          ...t,
          username: info?.username || 'Unnamed',
          avatarUrl: info?.avatarUrl || null,
        };
      });
      setMappedTrusts(mapped);
    }
  }, [state.getUsersResult]);

  // -----------------------------------------------------------------------------
  // Add Trust
  // -----------------------------------------------------------------------------

  useEffect(() => {
    switch (state.trustAddResult.type) {
      case 'loading':
      case 'notAsked':
      case 'failure':
        break;
      case 'success':
        setTimeout(() => {
          act(A._dashboard(A._getTrusts(unit)));
        }, 1000);
        break;
    }
  }, [state.trustAddResult]);

  // -----------------------------------------------------------------------------
  // Transfer
  // -----------------------------------------------------------------------------

  useEffect(() => {
    switch (state.transferResult.type) {
      case 'loading':
      case 'notAsked':
      case 'failure':
        break;
      case 'success':
        setTimeout(() => {
          act(A._dashboard(A._getBalance(unit)));
        }, 2000);
        break;
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
          <FadeIn orientation={'down'} delay={getDelay()}>
            <Icon path={mdiCog} size={1} color={theme.darkColor} />
          </FadeIn>
          <FadeIn orientation={'down'} delay={getDelay()}>
            <UserHandle>{`@${state.user.username}`}</UserHandle>
          </FadeIn>
          <FadeIn orientation={'down'} delay={getDelay()}>
            <Icon path={mdiLogout} size={1} color={theme.darkColor} />
          </FadeIn>
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
            <>
              <Button
                prio="high"
                color={theme.baseColor}
                onClick={() => toggleOverlay('SEND')}
              >
                <JustifyBetweenCenter>
                  <ButtonText>Send</ButtonText>
                  <Icon path={mdiCashFast} size={1} color={'white'} />
                </JustifyBetweenCenter>
              </Button>
              <Button
                color={theme.baseColor}
                onClick={() => toggleOverlay('RECEIVE')}
              >
                <JustifyBetweenCenter>
                  <ButtonText>Receive</ButtonText>
                  <Icon path={mdiHandCoin} size={1} color={'white'} />
                </JustifyBetweenCenter>
              </Button>
            </>
          </FadeIn>
        </ControlContent>
      }
      mainContent={
        <MainContent>
          <FlexBox>
            <FlexItemGrow>
              {mappedTrusts && (
                <FadeIn orientation={'up'} delay={getDelay()}>
                  <TrustUserList
                    title={'Trust Network'}
                    content={mappedTrusts}
                    theme={theme}
                    icon={mdiLan}
                    setActiveOverlay={setActiveOverlay}
                    setOverlayOpen={setOverlayOpen}
                    setOverwriteTo={setOverwriteTo}
                    addTrust={to =>
                      act(A._dashboard(A._addTrustConnection(to)))
                    }
                    removeTrust={to =>
                      act(A._dashboard(A._removeTrustConnection(to)))
                    }
                  />
                </FadeIn>
              )}
            </FlexItemGrow>

            <FlexItemGrow>
              {mappedSearch && (
                <FadeIn orientation={'up'} delay={getDelay()}>
                  <TrustUserList
                    title={'Explore'}
                    content={mappedSearch}
                    theme={theme}
                    icon={mdiMagnify}
                    setActiveOverlay={setActiveOverlay}
                    setOverlayOpen={setOverlayOpen}
                    setOverwriteTo={setOverwriteTo}
                    addTrust={to =>
                      act(A._dashboard(A._addTrustConnection(to)))
                    }
                    removeTrust={to =>
                      act(A._dashboard(A._removeTrustConnection(to)))
                    }
                    actionRow={
                      <JustifyBetweenCenter>
                        <InputWrapper>
                          <Input
                            type="text"
                            value={search}
                            placeholder={'Search by username'}
                            onChange={e => setSearch(e.target.value)}
                            onKeyPress={e =>
                              e.key === 'Enter' &&
                              act(
                                A._dashboard(A._userSearch({ query: search }))
                              )
                            }
                          />
                        </InputWrapper>
                        <DebugButtonWrapper>
                          <Button
                            prio={'high'}
                            color={theme.baseColor}
                            state={mapResult(state.userSearchResult)}
                            onClick={() =>
                              act(
                                A._dashboard(A._userSearch({ query: search }))
                              )
                            }
                          >
                            {'Search'}
                          </Button>
                        </DebugButtonWrapper>
                      </JustifyBetweenCenter>
                    }
                  />
                </FadeIn>
              )}
            </FlexItemGrow>
          </FlexBox>
        </MainContent>
      }
      overlay={
        overlayOpen ? (
          <Overlay
            theme={theme}
            closeOverlay={() => setOverlayOpen(false)}
            content={
              <DashboardOverlay
                overlay={activeOverlay}
                setOverlayOpen={setOverlayOpen}
                closeOverlay={() => setOverlayOpen(false)}
                overwriteTo={overwriteTo}
                state={state}
                act={act}
                theme={theme}
              />
            }
          />
        ) : null
      }
      debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};

// -----------------------------------------------------------------------------
// UI / DashboardOverlay
// -----------------------------------------------------------------------------

type OverlayType = 'SEND' | 'RECEIVE';

type DashboardOverlayProps = SendProps & {
  overlay: OverlayType;
  setOverlayOpen: React.Dispatch<SetStateAction<boolean>>;
};

const DashboardOverlay = ({
  overlay,
  state,
  act,
  theme,
  setOverlayOpen,
  overwriteTo,
}: DashboardOverlayProps) => {
  switch (overlay) {
    case 'SEND':
      return (
        <Send
          closeOverlay={() => setOverlayOpen(false)}
          overwriteTo={overwriteTo}
          state={state}
          act={act}
          theme={theme}
        />
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

const HeaderContent = tw.div`flex justify-between items-center mx-4`;
const ControlContent = tw.div`m-2 lg:m-0 lg:my-2`;
const MainContent = tw.div`relative`;
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
