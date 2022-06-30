import React, { ReactElement, useContext, useEffect } from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '../i18n';
import {
  CirclesConfig,
  mkControl,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.TS';
import {
  CirclesState,
  init,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import { useStateMachine } from './useStateMachine';
import {
  AskUsername,
  InfoGeneral,
  AskEmail,
  InfoSecurity,
  MagicWords,
  Submit,
  Landing,
  Login,
  Trusts,
  Dashboard,
} from './views';

// Style
import { ThemeProvider, ThemeContext } from '../context/theme';
import { AnimProvider } from '../context/anim';
import { CirclesAction } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { env } from '../env';
import { DebugContext, DebugProvider } from '../context/debug';
import tw, { css, styled } from 'twin.macro';
import { Unit, unit } from '@circles-pink/state-machine/output/Data.Unit';
import { fromFpTsEither } from '../utils/fpTs';
import * as E from 'fp-ts/Either';

type Language = 'en' | 'de';

type Content = {};

type UserConfig = {
  email?: string | ((email: string) => void);
};

export type OnboardingProps = {
  initState?: CirclesState;
  lang?: Language;
  baseColor?: string;
  content?: Content;
  userConfig?: UserConfig;
};

export const Onboarding = (props: OnboardingProps) => {
  return (
    <ThemeProvider>
      <Frame>
        <OnboardingContent {...props} />
      </Frame>
    </ThemeProvider>
  );
};

const cfgDefaultRight: CirclesConfig = {
  extractEmail: fromFpTsEither(
    E.right((email: string) => () => {
      // Save the email somewhere...
      console.log(email);
      return unit;
    })
  ),
};

const cfgDefaultLeft: CirclesConfig = {
  extractEmail: fromFpTsEither(E.left('hello@world.de')),
};

type CirclesConfigResolved =
  | { extractEmail: { _tag: 'Left'; left: string } }
  | { extractEmail: { _tag: 'Right'; right: (_: string) => () => Unit } };

const getSkipStates = (cfg: CirclesConfig): CirclesState['type'][] => {
  const toSkip: CirclesState['type'][] = [];
  if ((cfg as unknown as CirclesConfigResolved).extractEmail._tag === 'Left') {
    toSkip.push('askEmail');
  }
  return toSkip;
};

type ViewProps = {
  state: CirclesState;
  act: (m: CirclesAction) => void;
  cfg: CirclesConfig;
};

const View = ({ state, act, cfg }: ViewProps): ReactElement | null => {
  const skip = getSkipStates(cfg);

  const [debugContext, setDebugContext] = useContext(DebugContext);

  if (typeof window !== 'undefined') {
    (window as any).magicDebug = () => {
      setDebugContext(!debugContext);
    };
  }

  switch (state.type) {
    case 'landing':
      return <Landing state={state.value} act={act} />;
    case 'infoGeneral':
      return <InfoGeneral state={state.value} act={act} skip={skip} />;
    case 'askUsername':
      return <AskUsername state={state.value} act={act} skip={skip} />;
    case 'askEmail':
      return <AskEmail state={state.value} act={act} skip={skip} />;
    case 'infoSecurity':
      return <InfoSecurity state={state.value} act={act} skip={skip} />;
    case 'magicWords':
      return <MagicWords state={state.value} act={act} skip={skip} />;
    case 'submit':
      return <Submit state={state.value} act={act} skip={skip} />;
    case 'login':
      return <Login state={state.value} act={act} />;
    case 'trusts':
      return <Trusts state={state.value} act={act} />;
    case 'dashboard':
      return <Dashboard state={state.value} act={act} />;
    default:
      return null;
  }
};

const mkCfg = (userCfg: UserConfig): CirclesConfig => {
  if (typeof userCfg.email === 'string') {
    return {
      extractEmail: fromFpTsEither(E.left(userCfg.email)),
    };
  }

  return {
    extractEmail: fromFpTsEither(
      E.right((email: string) => () => {
        if (userCfg && userCfg.email && typeof userCfg.email !== 'string') {
          userCfg.email(email);
        }
        return unit;
      })
    ),
  };
};

const OnboardingContent = ({
  initState,
  lang = 'en',
  baseColor,
  content = {},
  userConfig,
}: OnboardingProps): ReactElement => {
  const cfg =
    userConfig && userConfig.email ? mkCfg(userConfig) : cfgDefaultRight;

  const control = mkControl(env)(cfg);

  const [state, act] = (useStateMachine as any)(
    (initState as unknown as CirclesState) || (init as unknown as CirclesState),
    control
  );
  const [theme, setTheme] = useContext(ThemeContext);

  i18n.changeLanguage(lang);

  useEffect(() => {
    if (baseColor) setTheme({ ...theme, baseColor });
  }, [baseColor]);

  return (
    <AnimProvider state={state}>
      <I18nextProvider i18n={i18n}>
        <DebugProvider>
          <View state={state} act={act} cfg={cfg} />
        </DebugProvider>
      </I18nextProvider>
    </AnimProvider>
  );
};

// -----------------------------------------------------------------------------
// Frame
// -----------------------------------------------------------------------------

export const Frame = styled.div(() => [
  tw`box-border`,
  css`
    * {
      -webkit-box-sizing: border-box;
      -moz-box-sizing: border-box;
      box-sizing: border-box;
      font-family: sans-serif;
    }
  `,
]);
