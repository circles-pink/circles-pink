import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import React, { ReactElement, useContext, useEffect } from 'react';
import { Button } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { LandingState } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import tw from 'twin.macro';
import { LoadingCircles } from '../../components/LoadingCircles';

type LandingProps = {
  state: LandingState;
  act: (ac: A.CirclesAction) => void;
};

export const Landing = ({ state, act }: LandingProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'up';
  const getDelay = getIncrementor(0, 0.05);

  useEffect(() => {
    act(A._landing(A._checkForSession(unit)));
  }, []);

  if (
    state.checkSessionResult.type === 'notAsked' ||
    state.checkSessionResult.type === 'loading'
  ) {
    return (
      <DialogCard
        mainContent={
          <CenterElement>
            <LoadingCircles width={250} color={theme.baseColor} />
          </CenterElement>
        }
      />
    );
  }

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
      debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};

const CenterElement = tw.div`flex justify-around mb-8`;
