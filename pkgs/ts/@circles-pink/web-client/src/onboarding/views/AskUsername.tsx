import {
  CirclesState,
  RemoteData,
  UserData,
  _RemoteData,
  _StateMachine,
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
import { pipe } from 'fp-ts/lib/function';

const { _circlesAction, _askUsernameAction } = _StateMachine;

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
                act(
                  _circlesAction._askUsername(
                    _askUsernameAction._setUsername(e.target.value)
                  )
                )
              }
              onKeyPress={e =>
                e.key === 'Enter' &&
                act(_circlesAction._askUsername(_askUsernameAction._next(unit)))
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
            onClick={() =>
              act(_circlesAction._askUsername(_askUsernameAction._next(unit)))
            }
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

const invalidUsername = new RegExp(/[^A-Za-z0-9]+/);

export const mapStatusMessage = (
  trustState: RemoteData<unknown, unknown, unknown, { isValid: boolean }>,
  username: string
) =>
  pipe(
    trustState,
    _RemoteData.unRemoteData({
      onFailure: () =>
        invalidUsername.test(username)
          ? t('askUsername.validation.charFail')
          : username.length < 3 || username.length > 24
          ? t('askUsername.validation.lengthFail')
          : '',
      onLoading: () => '',
      onSuccess: ({ isValid }) =>
        isValid ? '' : t('askUsername.validation.availFail'),
      onNotAsked: () => '',
    })
  );
