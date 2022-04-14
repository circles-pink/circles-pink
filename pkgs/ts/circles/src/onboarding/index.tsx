import React, {
  ReactElement,
  ReactNode,
  useContext,
  useEffect,
  useState,
} from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '../../i18n';
import { mkControl } from 'generated/output/CirclesPink.Garden.TS';
import {
  CirclesState,
  init,
} from 'generated/output/CirclesPink.Garden.StateMachine.State';
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
import '../styles/global.css';
import { ThemeProvider, ThemeContext } from '../context/theme';
import { AnimProvider } from '../context/anim';
import { CirclesAction } from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { env } from '../env';

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
      <OnboardingContent {...props} />
    </ThemeProvider>
  );
};

const control = mkControl(env);

type ViewProps = {
  state: CirclesState;
  act: (m: CirclesAction) => void;
};

const View = ({ state, act }: ViewProps): ReactElement | null => {
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
  const [theme, setColor] = useContext(ThemeContext);

  i18n.changeLanguage(lang);

  useEffect(() => {
    if (baseColor) setColor(baseColor);
  }, [baseColor]);

  return (
    <AnimProvider state={state}>
      <I18nextProvider i18n={i18n}>
        <View state={state} act={act} />
      </I18nextProvider>
    </AnimProvider>
  );
};
