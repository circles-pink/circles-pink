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
import { TrustNetworkList } from '../../components/TrustNetworkList';

type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
};

const mapBalance = (raw: string) => {
  const rawBalance = parseInt(raw);
  return Math.floor(rawBalance / 10000000000000000) / 100;
};

export const Dashboard = ({ state, act }: DashboardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const [addTrust, setAddTrust] = useState<string>('');
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  useEffect(() => {
    act(A._dashboard(A._getTrusts(unit))); // Should be done in control
    const pollTrusts = window.setInterval(
      () => act(A._dashboard(A._getTrusts(unit))),
      15000
    );
    return () => window.clearInterval(pollTrusts);
  }, []);

  useEffect(() => {
    switch (state.trustAddResult.type) {
      case 'loading':
      case 'notAsked':
      case 'failure':
        break;
      case 'success':
        setAddTrust('');
        act(A._dashboard(A._getTrusts(unit)));
        break;
    }
  }, [state.trustAddResult]);

  // const graph: Graph = new Map([[state.user.safeAddress, state.trusts]]);

  return (
    <UserDashboard
      header={
        <JustifyBetween>
          <FadeIn orientation={'down'} delay={getDelay()}>
            <Icon path={mdiCog} size={1} color={theme.darkColor} />
          </FadeIn>
          <FadeIn orientation={'down'} delay={getDelay()}>
            <UserHandle>{`@${state.user.username}`}</UserHandle>
          </FadeIn>
          <FadeIn orientation={'down'} delay={getDelay()}>
            <Icon path={mdiLogout} size={1} color={theme.darkColor} />
          </FadeIn>
        </JustifyBetween>
      }
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <BalanceWrapper>
              <Amount color={theme.baseColor}>
                {state.getBalanceResult.type === 'success'
                  ? mapBalance(state.getBalanceResult.value.toString())
                  : 0}
              </Amount>
              <CirclesCurrency color={theme.baseColor} />
            </BalanceWrapper>
          </FadeIn>

          {/* <FadeIn orientation={orientation} delay={getDelay()}>
            <TrustGraph graph={graph} />
          </FadeIn> */}
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <ControlContent>
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
          </ControlContent>
        </FadeIn>
      }
      mainContent={
        <>
          <FlexBox>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <TrustNetworkList
                title={'Trust Network'}
                content={state.trusts}
                theme={theme}
              />
            </FadeIn>
            {/* <FadeIn orientation={'up'} delay={getDelay()}>
              <ListElement title={'Transactions'} />
            </FadeIn>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <ListElement title={'Explore'} />
            </FadeIn> */}
          </FlexBox>
        </>
      }
      debug={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
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

const DebugOptionsTitle = tw.h2`text-xl`;
const ButtonText = tw.span`mr-3`;
const DebugButtonWrapper = tw.span`mb-3`;
const DebugOptionsDescription = tw.h2`text-sm text-gray-400`;
const ActionRow = tw.div`flex justify-between items-center`;
const InputWrapper = tw.div`pr-2 w-4/5`;
const FlexBox = tw.div`flex flex-col justify-between mb-4`;
const ControlContent = tw.div`m-2`;
const JustifyBetween = tw.div`flex justify-between items-center mx-4`;
const JustifyAround = tw.div`flex justify-around`;
