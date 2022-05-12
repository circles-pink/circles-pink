import React, { ReactElement, useContext, useEffect } from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '../i18n';
import { mkControl } from '@circles-pink/state-machine/output/CirclesPink.Garden.TS';
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
import { Global, css } from '@emotion/react';
import '../styles/global.css';

type Language = 'en' | 'de';

type Content = {};

type OnboardingProps = {
  initState?: CirclesState;
  lang?: Language;
  baseColor?: string;
  content?: Content;
};

export const Onboarding = (props: OnboardingProps) => {
  return (
    <ThemeProvider>
      <>
        <Global
          styles={css`
            * {
              box-sizing: border-box;
              font-family: sans-serif;
              font-size: 1em;
            }

            button {
              outline: none;
              border: none;
              padding: 0;
              margin: 0;
            }
          `}
        />
        <OnboardingContent {...props} />
      </>
    </ThemeProvider>
  );
};

const control = mkControl(env);

type ViewProps = {
  state: CirclesState;
  act: (m: CirclesAction) => void;
};

const View = ({ state, act }: ViewProps): ReactElement | null => {
  const [debugContext, setDebugContext] = useContext(DebugContext);

  (window as any).magicDebug = () => {
    setDebugContext(!debugContext);
  };

  switch (state.type) {
    case 'landing':
      return <Landing state={state.value} act={act} />;
    case 'infoGeneral':
      return <InfoGeneral state={state.value} act={act} />;
    case 'askUsername':
      return <AskUsername state={state.value} act={act} />;
    case 'askEmail':
      return <AskEmail state={state.value} act={act} />;
    case 'infoSecurity':
      return <InfoSecurity state={state.value} act={act} />;
    case 'magicWords':
      return <MagicWords state={state.value} act={act} />;
    case 'submit':
      return <Submit state={state.value} act={act} />;
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

const OnboardingContent = ({
  initState,
  lang = 'en',
  baseColor,
  content = {},
}: OnboardingProps): ReactElement => {
  const [state, act] = useStateMachine(initState || init, control);
  const [theme, setTheme] = useContext(ThemeContext);

  i18n.changeLanguage(lang);

  useEffect(() => {
    if (baseColor) setTheme({ ...theme, baseColor });
  }, [baseColor]);

  return (
    <AnimProvider state={state}>
      <I18nextProvider i18n={i18n}>
        <DebugProvider>
          <View state={state} act={act} />
        </DebugProvider>
      </I18nextProvider>
    </AnimProvider>
  );
};
