import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useContext } from 'react';
import { Button } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { LandingState } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';

type LandingProps = {
  state: LandingState;
  act: (ac: A.CirclesAction) => void;
};

export const Landing = ({ state, act }: LandingProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'up';
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('landing.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('landing.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            <Button
              prio={'medium'}
              color={theme.baseColor}
              onClick={() => act(A._landing(A._signIn(unit)))}
            >
              {t('signInButton')}
            </Button>
            <Button
              prio={'high'}
              color={theme.baseColor}
              onClick={() => act(A._landing(A._signUp(unit)))}
            >
              {t('signUpButton')}
            </Button>
          </>
        </FadeIn>
      }
      // debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};
