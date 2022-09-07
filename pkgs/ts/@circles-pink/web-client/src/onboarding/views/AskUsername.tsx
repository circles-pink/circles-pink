import {
  UserData,
} from '@circles-pink/state-machine/src';
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
import { OnboardingStepIndicator } from '../../components/OnboardingStepIndicator';
import { Status, StatusContainer, TwoButtonRow } from '../../components/helper';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';

type AskUsernameProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
  skip: CirclesState['type'][];
};

export const AskUsername = ({
  state,
  act,
  skip,
}: AskUsernameProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      header={<OnboardingStepIndicator skipStates={skip} />}
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
          <>
            <Input
              autoFocus
              indicatorColor={mapIndicatorColors(state.usernameApiResult)}
              type="text"
              value={state.username}
              placeholder={t('askUsername.usernamePlaceholder')}
              onChange={e =>
                act(A._askUsername(A._setUsername(e.target.value)))
              }
              onKeyPress={e =>
                e.key === 'Enter' && act(A._askUsername(A._next(unit)))
              }
            />
            <StatusContainer>
              <Status>
                {mapStatusMessage(state.usernameApiResult, state.username)}
              </Status>
            </StatusContainer>
          </>
        </FadeIn>
      }
      // debug={<pre>{JSON.stringify(state.usernameApiResult, null, 2)}</pre>}
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <Button
            prio={'high'}
            theme={theme}
            onClick={() => act(A._askUsername(A._next(unit)))}
          >
            {t('nextButton')}
          </Button>
        </FadeIn>
      }
      debug={<StateMachineDebugger state={state} />}
    />
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const mapStatusMessage = (trustState: UsernameApiResult, username: string) => {
  switch (trustState.type) {
    case 'loading':
      return '';
    case 'success':
      if (trustState.value.isValid) {
        return '';
      }
      return t('askUsername.validation.availFail');
    case 'failure':
      const regex = new RegExp(/[^A-Za-z0-9]+/);
      if (regex.test(username)) {
        return t('askUsername.validation.charFail');
      }
      if (username.length < 3 || username.length > 24) {
        return t('askUsername.validation.lengthFail');
      }
    case 'notAsked':
      return '';
  }
};
