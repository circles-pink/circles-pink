import {
  CirclesState,
  UserData,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement, useContext } from 'react';
import { Button } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { OnboardingStepIndicator } from '../../components/OnboardingStepIndicator';
import { mapResult } from '../utils/mapResult';
import { TwoButtonRow } from '../../components/helper';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';

type SubmitProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
  skip: CirclesState['type'][];
};

export const Submit = ({ state, act, skip }: SubmitProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      header={<OnboardingStepIndicator skipStates={skip} />}
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>
              {t('submit.claim').replace('{{user}}', `"${state.username}"`)}
            </Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('submit.subClaim')}</SubClaim>
          </FadeIn>

          {/* <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>
              {t('submit.username')} {state.username}
            </SubClaim>
          </FadeIn> */}
          {/* {state.email && (
            <FadeIn orientation={orientation} delay={getDelay()}>
              <SubClaim>Email: {state.email}</SubClaim>
            </FadeIn>
          )} */}
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <TwoButtonRow>
            <Button
              prio={'medium'}
              theme={theme}
              onClick={() => act(A._submit(A._prev(unit)))}
            >
              {t('prevButton')}
            </Button>

            <Button
              prio={'high'}
              theme={theme}
              state={mapResult(state.submitResult)}
              onClick={() => act(A._submit(A._submit(unit)))}
            >
              {t('submitButton')}
            </Button>
          </TwoButtonRow>
        </FadeIn>
      }
      debug={<StateMachineDebugger state={state} />}
    />
  );
};
