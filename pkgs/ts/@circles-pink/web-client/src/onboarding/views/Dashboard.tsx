import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useContext, useEffect, useState } from 'react';
import { Button, Input } from '../../components/forms';
import { Text } from '../../components/text';
import { UserDashboard } from '../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { DashboardState } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { mapResult } from '../utils/mapResult';
import tw, { css, styled } from 'twin.macro';
import { mdiCashFast, mdiCog, mdiHandCoin, mdiLogout } from '@mdi/js';
import Icon from '@mdi/react';
import { CirclesCurrency } from '../../assets/CirclesCurrency';
import {
  MappedTrustNodes,
  TrustNetworkList,
} from '../../components/TrustNetworkList';
import { Balance } from 'generated/output/CirclesCore.Bindings';
import { User } from 'generated/output/CirclesCore';

// -----------------------------------------------------------------------------
// Dashboard
// -----------------------------------------------------------------------------

type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
};

export const Dashboard = ({ state, act }: DashboardProps): ReactElement => {
  // -----------------------------------------------------------------------------
  // State & Context
  // -----------------------------------------------------------------------------

  // Theme
  const [theme] = useContext(ThemeContext);

  // User Interaction
  // Add Trust
  const [addTrust, setAddTrust] = useState<string>('');
  // Transfer
  const [from, setFrom] = useState<string>('');
  const [to, setTo] = useState<string>('');
  const [value, setValue] = useState<number>(0);
  const [paymentNote, setPaymentNote] = useState<string>('');
  // Search
  const [search, setSearch] = useState<string>('');

  // animation
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  // -----------------------------------------------------------------------------
  // Side Effects
  // -----------------------------------------------------------------------------

  useEffect(() => {
    // Gather initial Client information
    act(A._dashboard(A._getBalance(unit)));
    act(A._dashboard(A._getTrusts(unit))); // Should be done in control
    act(A._dashboard(A._checkUBIPayout(unit)));

    // Setup polling intervals
    const pollTrusts = window.setInterval(
      () => act(A._dashboard(A._getTrusts(unit))),
      15 * 1000
    );
    const pollUBIPayout = window.setInterval(
      () => act(A._dashboard(A._checkUBIPayout(unit))),
      5 * 60 * 1000
    );

    // Clear polling intervals
    return () => {
      window.clearInterval(pollTrusts);
      window.clearInterval(pollUBIPayout);
    };
  }, []);

  // -----------------------------------------------------------------------------
  // Balance
  // -----------------------------------------------------------------------------
  const [balance, setBalance] = useState<string>('0.00');

  useEffect(() => {
    if (state.getBalanceResult.type === 'success') {
      setBalance(mapBalanceToHr(state.getBalanceResult.value));
    }
  }, [state.getBalanceResult]);

  // -----------------------------------------------------------------------------
  // UBI Payout
  // -----------------------------------------------------------------------------

  useEffect(() => {
    // If polling for checkPayout happens successfully
    // we can start a new payout request
    if (state.checkUBIPayoutResult.type === 'success') {
      act(A._dashboard(A._requestUBIPayout(unit)));
    }
  }, [state.checkUBIPayoutResult]);

  useEffect(() => {
    // Refresh balance after payout request with small timeout
    if (state.requestUBIPayoutResult.type === 'success') {
      setTimeout(() => {
        act(A._dashboard(A._getBalance(unit)));
      }, 2000);
    }
  }, [state.requestUBIPayoutResult]);

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
        setAddTrust('');
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
        setFrom('');
        setTo('');
        setValue(0);
        setPaymentNote('');
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
            <BalanceWrapper>
              <Amount color={theme.baseColor}>{balance}</Amount>
              <CirclesCurrency color={theme.baseColor} />
            </BalanceWrapper>
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
                // onClick={() => act(A._dashboard(A._getTrusts(unit)))}
              >
                <ActionRow>
                  <ButtonText>Send</ButtonText>
                  <Icon path={mdiCashFast} size={1} color={'white'} />
                </ActionRow>
              </Button>
              <Button
                color={theme.baseColor}
                // onClick={() => act(A._dashboard(A._getTrusts(unit)))}
              >
                <ActionRow>
                  <ButtonText>Receive</ButtonText>
                  <Icon path={mdiHandCoin} size={1} color={'white'} />
                </ActionRow>
              </Button>
            </>
          </FadeIn>
        </ControlContent>
      }
      mainContent={
        <>
          <FlexBox>
            <FlexItemGrow>
              {mappedTrusts && (
                <FadeIn orientation={'up'} delay={getDelay()}>
                  <TrustNetworkList
                    title={'Trust Network'}
                    content={mappedTrusts}
                    theme={theme}
                  />
                </FadeIn>
              )}
            </FlexItemGrow>

            {/* <FlexItemGrow>
              {mappedTrusts && (
                <FadeIn orientation={'up'} delay={getDelay()}>
                  <TrustNetworkList
                    title={'Explore'}
                    content={mappedTrusts}
                    theme={theme}
                  />
                </FadeIn>
              )}
            </FlexItemGrow> */}
          </FlexBox>
        </>
      }
      debug={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            {/* Trust AddConnection */}
            <DebugOptionsTitle>{t('dashboard.debugTitle')}</DebugOptionsTitle>
            <DebugOptionsDescription>
              trust.addConnection
            </DebugOptionsDescription>
            <ActionRow>
              <InputWrapper>
                <Input
                  type="text"
                  value={addTrust}
                  placeholder={t('dashboard.addTrustPlaceholder')}
                  onChange={e => setAddTrust(e.target.value)}
                  onKeyPress={e =>
                    e.key === 'Enter' &&
                    act(A._dashboard(A._addTrustConnection(addTrust)))
                  }
                />
              </InputWrapper>
              <DebugButtonWrapper>
                <Button
                  prio={'high'}
                  color={theme.baseColor}
                  state={mapResult(state.trustAddResult)}
                  onClick={() =>
                    act(A._dashboard(A._addTrustConnection(addTrust)))
                  }
                >
                  {t('addTrustsButton')}
                </Button>
              </DebugButtonWrapper>
            </ActionRow>
            <FlexBox>Own Safe Address: {state.user.safeAddress}</FlexBox>
            {/* Token Transfer */}
            <DebugOptionsDescription>token.transfer</DebugOptionsDescription>
            <ActionRow>
              <InputWrapper>
                <Input
                  type="text"
                  value={from}
                  placeholder={'From'}
                  onChange={e => setFrom(e.target.value)}
                  onKeyPress={e =>
                    e.key === 'Enter' &&
                    act(
                      A._dashboard(
                        A._transfer({
                          from,
                          to,
                          value: mapBalanceToBN(value),
                          paymentNote,
                        })
                      )
                    )
                  }
                />
                <Input
                  type="text"
                  value={to}
                  placeholder={'To'}
                  onChange={e => setTo(e.target.value)}
                  onKeyPress={e =>
                    e.key === 'Enter' &&
                    act(
                      A._dashboard(
                        A._transfer({
                          from,
                          to,
                          value: mapBalanceToBN(value),
                          paymentNote,
                        })
                      )
                    )
                  }
                />
                <Input
                  type="number"
                  value={value}
                  placeholder={'Amount'}
                  onChange={e => setValue(parseInt(e.target.value))}
                  onKeyPress={e =>
                    e.key === 'Enter' &&
                    act(
                      A._dashboard(
                        A._transfer({
                          from,
                          to,
                          value: mapBalanceToBN(value),
                          paymentNote,
                        })
                      )
                    )
                  }
                />
                <Input
                  type="string"
                  value={paymentNote}
                  placeholder={'Payment Note'}
                  onChange={e => setPaymentNote(e.target.value)}
                  onKeyPress={e =>
                    e.key === 'Enter' &&
                    act(
                      A._dashboard(
                        A._transfer({
                          from,
                          to,
                          value: mapBalanceToBN(value),
                          paymentNote,
                        })
                      )
                    )
                  }
                />
              </InputWrapper>
              <DebugButtonWrapper>
                <Button
                  prio={'high'}
                  color={theme.baseColor}
                  state={mapResult(state.transferResult)}
                  onClick={() =>
                    act(
                      A._dashboard(
                        A._transfer({
                          from,
                          to,
                          value: mapBalanceToBN(value),
                          paymentNote,
                        })
                      )
                    )
                  }
                >
                  Send
                </Button>
              </DebugButtonWrapper>
            </ActionRow>
            {JSON.stringify(state.transferResult, null, 2)}
            {/* User Search*/}
            {/* Trust AddConnection */}
            <DebugOptionsDescription>user.search</DebugOptionsDescription>
            <ActionRow>
              <InputWrapper>
                <Input
                  type="text"
                  value={search}
                  placeholder={'Search by username'}
                  onChange={e => setSearch(e.target.value)}
                  onKeyPress={e =>
                    e.key === 'Enter' &&
                    act(A._dashboard(A._userSearch({ query: search })))
                  }
                />
              </InputWrapper>
              <DebugButtonWrapper>
                <Button
                  prio={'high'}
                  color={theme.baseColor}
                  state={mapResult(state.userSearchResult)}
                  onClick={() =>
                    act(A._dashboard(A._userSearch({ query: search })))
                  }
                >
                  {'Search'}
                </Button>
              </DebugButtonWrapper>
            </ActionRow>
            {JSON.stringify(state.userSearchResult, null, 2)}
          </>
        </FadeIn>
      }
    />
  );
};

// -----------------------------------------------------------------------------
// UI / Balance
// -----------------------------------------------------------------------------

type AmountProps = { color?: string };

const Amount = styled.h2<AmountProps>(({ color }) => [
  tw`text-5xl mr-2 my-2`,
  css`
    color: ${color || 'black'};
  `,
]);
const BalanceWrapper = tw.div`flex flex-row items-center m-2`;

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

const ControlContent = tw.div`m-2 lg:m-0 lg:my-2`;
const HeaderContent = tw.div`flex justify-between items-center mx-4`;
const DebugOptionsTitle = tw.h2`text-xl`;
const ButtonText = tw.span`mr-3`;
const DebugButtonWrapper = tw.span`mb-3`;
const DebugOptionsDescription = tw.h2`text-sm text-gray-400`;
const ActionRow = tw.div`flex justify-between items-center`;
const InputWrapper = tw.div`pr-2 w-4/5`;
const FlexBox = tw.div`flex flex-wrap lg:flex-row flex-col justify-between mb-4 gap-4 mx-2`;
const FlexItemGrow = tw.div`flex-grow`;
const JustifyAround = tw.div`flex justify-around`;

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const mapBalanceToHr = (raw: Balance) => {
  const rawBalance = parseInt(raw.toString());
  // Map and round balance to human readable format
  return (
    Math.floor(rawBalance / 1000 / 1000 / 1000 / 1000 / 1000 / 10) / 100
  ).toFixed(2);
};

const mapBalanceToBN = (raw: number) => {
  const rawBalance = (raw * 100).toString();
  // Map and round balance to big number format
  // Todo: Replace! Ugly but works for the moment...
  return rawBalance + '0000000000000000';
};
