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
import { OnboardingStepIndicator } from '../../components/layout';
import { TwoButtonRow } from '../../components/helper';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';

type InfoSecurityProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
  skip: CirclesState['type'][];
};

export const InfoSecurity = ({
  state,
  act,
  skip,
}: InfoSecurityProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      header={<OnboardingStepIndicator skipStates={skip} />}
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('infoSecurity.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('infoSecurity.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <TwoButtonRow>
            <Button
              theme={theme}
              prio={'medium'}
              onClick={() => act(A._infoSecurity(A._prev(unit)))}
            >
              {t('prevButton')}
            </Button>

            <Button
              prio={'high'}
              theme={theme}
              onClick={() => act(A._infoSecurity(A._next(unit)))}
            >
              {t('nextButton')}
            </Button>
          </TwoButtonRow>
        </FadeIn>
      }
      debug={<StateMachineDebugger state={state} />}
    />
  );
};
