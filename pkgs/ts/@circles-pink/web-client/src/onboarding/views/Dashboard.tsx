import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useContext, useEffect, useState } from 'react';
import { Button, Input } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { UserDashboard } from '../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { DashboardState } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { mapResult } from '../utils/mapResult';
import tw from 'twin.macro';
import { ListElement } from '../../components/ListElement';
import { mdiCashFast, mdiCashPlus } from '@mdi/js';
import Icon from '@mdi/react';

type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
};

export const Dashboard = ({ state, act }: DashboardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const [addTrust, setAddTrust] = useState<string>('');
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  useEffect(() => {
    act(A._dashboard(A._getTrusts(unit))); // Should be done in control
    setInterval(() => act(A._dashboard(A._getTrusts(unit))), 15000);
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
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            {state.user.username ? (
              <Claim color={theme.baseColor}>
                {t('dashboard.greet')}
                {` ${state.user.username}!`}
              </Claim>
            ) : (
              <Claim color={theme.baseColor}>{t('dashboard.claim')}</Claim>
            )}
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('dashboard.subClaim')}</SubClaim>
          </FadeIn>

          {/* <FadeIn orientation={orientation} delay={getDelay()}>
            <TrustGraph graph={graph} />
          </FadeIn> */}
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
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
                <Icon path={mdiCashPlus} size={1} color={'white'} />
              </ActionRow>
            </Button>
            {/* <Button
              color={theme.baseColor}
              onClick={() => act(A._dashboard(A._getTrusts(unit)))}
            >
              {t('getTrustsButton')}
            </Button> */}
          </>
        </FadeIn>
      }
      mainContent={
        <>
          <FlexRow>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <ListElement title={'Trust Network'} />
            </FadeIn>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <ListElement title={'Transactions'} />
            </FadeIn>
            <FadeIn orientation={'up'} delay={getDelay()}>
              <ListElement title={'Explore'} />
            </FadeIn>
          </FlexRow>
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
              </ActionRow>
            </>
          </FadeIn>
        </>
      }
      // debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};

const DebugOptionsTitle = tw.h2`text-xl`;
const ButtonText = tw.span`mr-2`;
const DebugOptionsDescription = tw.h2`text-sm text-gray-400`;
const ActionRow = tw.div`flex justify-between items-center`;
const InputWrapper = tw.div`pr-2 w-4/5`;
const FlexRow = tw.div`flex lg:flex-row flex-col justify-between mb-4`;
