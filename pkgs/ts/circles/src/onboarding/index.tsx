import React, { ReactElement, useContext, useEffect } from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '../../i18n';
import { control } from 'generated/output/CirclesPink.Garden.TS';
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
} from './views';

// Style
import '../styles/global.css';
import {
  ThemeProvider,
  ThemeContext,
  ThemeContextType,
} from '../context/theme';
import chroma from 'chroma-js';

type Language = 'en' | 'de';

type OnboardingProps = {
  initState?: CirclesState;
  lang?: Language;
  baseColor?: string;
};

export const Onboarding = (props: OnboardingProps) => {
  return (
    <ThemeProvider>
      <OnboardingContent {...props} />
    </ThemeProvider>
  );
};

const OnboardingContent = ({
  initState,
  lang = 'en',
  baseColor,
}: OnboardingProps): ReactElement => {
  const [theme, setColor] = useContext(ThemeContext);
  const [state, act] = useStateMachine(initState || init, control);
  i18n.changeLanguage(lang);

  useEffect(() => {
    if (baseColor) setColor(baseColor);
  }, [baseColor]);

  return (
    <I18nextProvider i18n={i18n}>
      {{
        infoGeneral: () => <InfoGeneral state={state.value} act={act} />,
        askUsername: () => <AskUsername state={state.value} act={act} />,
        askEmail: () => <AskEmail state={state.value} act={act} />,
        infoSecurity: () => <InfoSecurity state={state.value} act={act} />,
        magicWords: () => <MagicWords state={state.value} act={act} />,
        dashboard: () => null,
        submit: () => null,
      }[state.type]()}
    </I18nextProvider>
  );
};
