import { UserData } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
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
import { mapResult } from '../utils/mapResult';
import { TwoButtonRow } from '../../components/helper';

type SubmitProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const Submit = ({ state, act }: SubmitProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      header={<OnboardingStepIndicator />}
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('submit.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('submit.subClaim')}</SubClaim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>
              Username: {state.username}
              <br />
              Email: {state.email}
            </SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <TwoButtonRow>
            <Button
              prio={'medium'}
              color={theme.baseColor}
              onClick={() => act(A._submit(A._prev(unit)))}
            >
              {t('prevButton')}
            </Button>

            <Button
              prio={'high'}
              color={theme.baseColor}
              state={mapResult(state.submitResult)}
              onClick={() => act(A._submit(A._submit(unit)))}
            >
              {t('submitButton')}
            </Button>
          </TwoButtonRow>
        </FadeIn>
      }
      debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};
