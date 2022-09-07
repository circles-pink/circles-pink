import React, { ReactElement, useContext } from 'react';
import { Button, Input } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { Status, StatusContainer, TwoButtonRow } from '../../components/helper';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import {
  CirclesAction,
  LoginState,
  unit,
  _Maybe,
  _RemoteData,
  _StateMachine,
} from '@circles-pink/state-machine/src';
import { mapResult } from '../utils/mapResult';
import { unRemoteData } from '@circles-pink/state-machine/output/RemoteData';
import { pipe } from 'fp-ts/lib/function';
import { matchV } from '../../purs-util';

type LoginProps = {
  state: LoginState;
  act: (ac: CirclesAction) => void;
};

export const Login = ({ state, act }: LoginProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'up';
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('login.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('login.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      interaction={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            <Input
              autoFocus
              // indicatorColor={mapIndicatorColors(state.usernameApiResult)}
              type="password"
              value={state.magicWords}
              placeholder={t('login.magicWordsPlaceholder')}
              onChange={e =>
                act(
                  _StateMachine._circlesAction._login(
                    _StateMachine._loginAction._setMagicWords(e.target.value)
                  )
                )
              }
              onKeyPress={e => {
                if (e.key === 'Enter') {
                  act(
                    _StateMachine._circlesAction._login(
                      _StateMachine._loginAction._setMagicWords(
                        state.magicWords.trim()
                      )
                    )
                  );
                  act(
                    _StateMachine._circlesAction._login(
                      _StateMachine._loginAction._login(unit)
                    )
                  );
                }
              }}
            />
            {mapStatusMessage(state.loginResult) !== '' && (
              <StatusContainer>
                <Status>{mapStatusMessage(state.loginResult)}</Status>
              </StatusContainer>
            )}
          </>
        </FadeIn>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <TwoButtonRow>
            <Button
              prio={'high'}
              theme={theme}
              state={mapResult(state.loginResult)}
              onClick={() => {
                act(
                  _StateMachine._circlesAction._login(
                    _StateMachine._loginAction._setMagicWords(
                      state.magicWords.trim()
                    )
                  )
                );
                act(
                  _StateMachine._circlesAction._login(
                    _StateMachine._loginAction._login(unit)
                  )
                );
              }}
            >
              {t('signInSubmitButton')}
            </Button>
            <Button
              prio={'medium'}
              theme={theme}
              onClick={() =>
                act(
                  _StateMachine._circlesAction._login(
                    _StateMachine._loginAction._signUp(unit)
                  )
                )
              }
            >
              {t('signUpInsteadButton')}
            </Button>
          </TwoButtonRow>
        </FadeIn>
      }
      debug={<StateMachineDebugger state={state} />}
    />
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const mapStatusMessage = (loginResult: LoginState['loginResult']) =>
  pipe(
    loginResult,
    _RemoteData.unRemoteData({
      onLoading: () => '',
      onSuccess: () => '',
      onFailure: ({ error }) =>
        matchV(error)(
          {
            errInvalidMnemonic: () => t('login.validation.invalidMnemonic'),
            errUserNotFound: () => t('login.validation.userNotFound'),
          },
          () => ''
        ),
      onNotAsked: () => '',
    })
  );
