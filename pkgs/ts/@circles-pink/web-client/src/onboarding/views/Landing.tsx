import React, { ReactElement, useContext, useEffect } from 'react';
import { Button, ButtonLinkLike } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import tw from 'twin.macro';
import { LoadingCircles } from '../../components/LoadingCircles';
import {
  JustifyBetween,
  JustifyStartCenter,
  TwoButtonRow,
} from '../../components/helper';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import { LandingState } from '@circles-pink/state-machine/src';

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
          {/* <TwoButtonRow>
            <Button
              prio={'medium'}
              theme={theme}
              onClick={() => act(A._landing(A._signIn(unit)))}
            >
              {t('signInButton')}
            </Button>
            <Button
              prio={'high'}
              theme={theme}
              onClick={() => act(A._landing(A._signUp(unit)))}
            >
              {t('signUpButton')}
            </Button>
          </TwoButtonRow> */}
          <Button
            prio={'high'}
            theme={theme}
            onClick={() => act(A._landing(A._signUp(unit)))}
          >
            {t('signUpButton')}
          </Button>
        </FadeIn>
      }
      mainContent={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <JustifyStartCenter>
            <SubClaim>{t('landing.loginWithKey')}</SubClaim>
            <ButtonLinkLike onClick={() => act(A._landing(A._signIn(unit)))}>
              {t('signInButton')}
            </ButtonLinkLike>
          </JustifyStartCenter>
        </FadeIn>
      }
      debug={<StateMachineDebugger state={state} />}
    />
  );
};

const CenterElement = tw.div`flex justify-around mb-8`;
