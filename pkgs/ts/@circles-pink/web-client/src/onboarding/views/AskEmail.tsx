import React, { ReactElement, useContext } from 'react';
import { DialogCard } from '../../components/DialogCard';
import { Claim, SubClaim, Text } from '../../components/text';
import { Button, Input, Checkbox } from '../../components/forms';
import { mapIndicatorColors } from '../utils/mapIndicatorColors';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { OnboardingStepIndicator } from '../../components/OnboardingStepIndicator';
import { Status, StatusContainer, TwoButtonRow } from '../../components/helper';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import tw, { styled } from 'twin.macro';
import {
  CirclesAction,
  CirclesState,
  RemoteData,
  unit,
  UserData,
  _RemoteData,
  _StateMachine,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';

const { _circlesAction, _askEmailAction } = _StateMachine;

type AskEmailProps = {
  state: UserData;
  act: (ac: CirclesAction) => void;
  skip: CirclesState['type'][];
};

export const AskEmail = ({ state, act, skip }: AskEmailProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);
  const [theme] = useContext(ThemeContext);

  return (
    <DialogCard
      header={<OnboardingStepIndicator skipStates={skip} />}
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
            <InputContainer>
              <Input
                autoFocus
                indicatorColor={mapIndicatorColors(state.emailApiResult)}
                type="text"
                value={state.email}
                placeholder={t('askEmail.emailPlaceholder')}
                onChange={e =>
                  act(
                    _circlesAction._askEmail(
                      _askEmailAction._setEmail(e.target.value)
                    )
                  )
                }
                onKeyPress={e =>
                  e.key === 'Enter' &&
                  act(_circlesAction._askEmail(_askEmailAction._next(unit)))
                }
              />
              {mapStatusMessage(state.emailApiResult) !== '' && (
                <StatusContainer>
                  <Status>{mapStatusMessage(state.emailApiResult)}</Status>
                </StatusContainer>
              )}
            </InputContainer>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <>
              <div>
                <Checkbox
                  background={theme.baseColor}
                  borderColor={theme.baseColor}
                  label={t('askEmail.termsLabel')}
                  checked={state.terms}
                  setChecked={() =>
                    act(
                      _circlesAction._askEmail(_askEmailAction._setTerms(unit))
                    )
                  }
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
                  setChecked={() =>
                    act(
                      _circlesAction._askEmail(
                        _askEmailAction._setPrivacy(unit)
                      )
                    )
                  }
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
              onClick={() =>
                act(_circlesAction._askEmail(_askEmailAction._prev(unit)))
              }
            >
              {t('prevButton')}
            </Button>

            <Button
              prio={'high'}
              theme={theme}
              onClick={() =>
                act(_circlesAction._askEmail(_askEmailAction._next(unit)))
              }
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

const InputContainer = styled.div(() => [tw`mb-2`]);

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

export const mapStatusMessage = (
  rd: RemoteData<unknown, unknown, unknown, { isValid: boolean }>
) =>
  pipe(
    rd,
    _RemoteData.unRemoteData({
      onFailure: () => t('askEmail.validation.email'),
      onLoading: () => '',
      onSuccess: ({ isValid }) => '',
      onNotAsked: () => '',
    })
  );
