import { UserData } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement, useContext } from 'react';
import { DialogCard } from '../../components/DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { Button, Input } from '../../components/forms';
import { mapIndicatorColors } from '../utils/mapIndicatorColors';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';
import { t } from 'i18next';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { ThemeContext } from '../../context/theme';
import { OnboardingStepIndicator } from '../../components/layout';
import { TwoButtonRow } from '../../components/helper';

type AskUsernameProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskUsername = ({ state, act }: AskUsernameProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      header={<OnboardingStepIndicator />}
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('askUsername.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('askUsername.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      interaction={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <Input
            autoFocus
            indicatorColor={mapIndicatorColors(state.usernameApiResult)}
            type="text"
            value={state.username}
            placeholder={t('askUsername.usernamePlaceholder')}
            onChange={e => act(A._askUsername(A._setUsername(e.target.value)))}
            onKeyPress={e =>
              e.key === 'Enter' && act(A._askUsername(A._next(unit)))
            }
          />
        </FadeIn>
      }
      // debug={<pre>{JSON.stringify(state.usernameApiResult, null, 2)}</pre>}
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <TwoButtonRow>
            <Button
              theme={theme}
              prio={'medium'}
              onClick={() => act(A._askUsername(A._prev(unit)))}
            >
              {t('prevButton')}
            </Button>

            <Button
              prio={'high'}
              theme={theme}
              onClick={() => act(A._askUsername(A._next(unit)))}
            >
              {t('nextButton')}
            </Button>
          </TwoButtonRow>
        </FadeIn>
      }
      debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};
