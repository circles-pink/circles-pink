import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import React, { ReactElement, useContext } from 'react';
import { Button, Input } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import {
  LoginState,
  LoginStateLoginResult,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Login';
import { ErrLoginStateResolved } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Login.Views';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { mapResult } from '../utils/mapResult';
import { Status, StatusContainer, TwoButtonRow } from '../../components/helper';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';

type LoginProps = {
  state: LoginState;
  act: (ac: A.CirclesAction) => void;
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
              onChange={e => act(A._login(A._setMagicWords(e.target.value)))}
              onKeyPress={e => {
                if (e.key === 'Enter') {
                  act(A._login(A._setMagicWords(state.magicWords.trim())));
                  act(A._login(A._login(unit)));
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
                act(A._login(A._setMagicWords(state.magicWords.trim())));
                act(A._login(A._login(unit)));
              }}
            >
              {t('signInSubmitButton')}
            </Button>
            <Button
              prio={'medium'}
              theme={theme}
              onClick={() => act(A._login(A._signUp(unit)))}
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

const mapStatusMessage = (loginResult: LoginStateLoginResult) => {
  switch (loginResult.type) {
    case 'loading':
      return '';
    case 'success':
      return '';
    case 'failure': {
      switch ((loginResult.value.error as ErrLoginStateResolved).type) {
        case 'errInvalidMnemonic':
          return t('login.validation.invalidMnemonic');
        case 'errUserNotFound':
          return t('login.validation.userNotFound');
      }
    }
    case 'notAsked':
      return '';
  }
};
