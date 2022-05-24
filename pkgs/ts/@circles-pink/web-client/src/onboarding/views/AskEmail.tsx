import {
  EmailApiResult,
  UserData,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement, useContext } from 'react';
import { DialogCard } from '../../components/DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { Button, Input, Checkbox } from '../../components/forms';
import { mapIndicatorColors } from '../utils/mapIndicatorColors';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { OnboardingStepIndicator } from '../../components/layout';
import { TwoButtonRow } from '../../components/helper';
import tw from 'twin.macro';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';

type AskEmailProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskEmail = ({ state, act }: AskEmailProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);
  const [theme] = useContext(ThemeContext);

  return (
    <DialogCard
      header={<OnboardingStepIndicator />}
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('askEmail.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('askEmail.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      interaction={
        <>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <>
              <Input
                autoFocus
                indicatorColor={mapIndicatorColors(state.emailApiResult)}
                type="text"
                value={state.email}
                placeholder={t('askEmail.emailPlaceholder')}
                onChange={e => act(A._askEmail(A._setEmail(e.target.value)))}
                onKeyPress={e =>
                  e.key === 'Enter' && act(A._askEmail(A._next(unit)))
                }
              />
              {mapStatusMessage(state.emailApiResult) !== '' && (
                <Status>{mapStatusMessage(state.emailApiResult)}</Status>
              )}
            </>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <>
              <div>
                <Checkbox
                  background={theme.baseColor}
                  borderColor={theme.baseColor}
                  label={t('askEmail.termsLabel')}
                  checked={state.terms}
                  setChecked={() => act(A._askEmail(A._setTerms(unit)))}
                />
              </div>
              {!state.terms && (
                <StatusContainer>
                  <Status>{t('askEmail.validation.terms')}</Status>
                </StatusContainer>
              )}
            </>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <>
              <div>
                <Checkbox
                  background={theme.baseColor}
                  borderColor={theme.baseColor}
                  label={t('askEmail.privacyLabel')}
                  checked={state.privacy}
                  setChecked={() => act(A._askEmail(A._setPrivacy(unit)))}
                />
              </div>
              {!state.privacy && (
                <StatusContainer>
                  <Status>{t('askEmail.validation.privacy')}</Status>
                </StatusContainer>
              )}
            </>
          </FadeIn>
        </>
      }
      // debug={<pre>{JSON.stringify(state.emailApiResult, null, 2)}</pre>}
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <TwoButtonRow>
            <Button
              theme={theme}
              prio={'medium'}
              onClick={() => act(A._askEmail(A._prev(unit)))}
            >
              {t('prevButton')}
            </Button>

            <Button
              prio={'high'}
              theme={theme}
              onClick={() => act(A._askEmail(A._next(unit)))}
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

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const StatusContainer = tw.div`py-2`;
const Status = tw.div``;

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const mapStatusMessage = (email: EmailApiResult) => {
  switch (email.type) {
    case 'loading':
      return '';
    case 'success':
      return '';
    case 'failure':
      return t('askEmail.validation.email');
    case 'notAsked':
      return '';
  }
};
