import React, { ReactElement, useContext, useEffect, useState } from 'react';
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
} from './views';

// Style
import '../styles/global.css';
import { ThemeProvider, ThemeContext } from '../context/theme';
import { AnimProvider } from '../context/anim';

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

const control = mkControl(
  process.env.STORYBOOK_GARDEN_API !== undefined &&
    process.env.STORYBOOK_GARDEN_API_USERS !== undefined &&
    process.env.STORYBOOK_GRAPH_API !== undefined &&
    process.env.STORYBOOK_GARDEN_SUBGRAPH_NAME !== undefined &&
    process.env.STORYBOOK_GARDEN_RELAY !== undefined &&
    process.env.STORYBOOK_GARDEN_HUB_ADDRESS !== undefined &&
    process.env.STORYBOOK_GARDEN_PROXY_FACTORY_ADRESS !== undefined &&
    process.env.STORYBOOK_GARDEN_SAFE_MASTER_ADDRESS !== undefined
    ? {
        gardenApi: process.env.STORYBOOK_GARDEN_API,
        gardenApiUsers: process.env.STORYBOOK_GARDEN_API_USERS,
        gardenGraphApi: process.env.STORYBOOK_GRAPH_API,
        gardenSubgraphName: process.env.STORYBOOK_GARDEN_SUBGRAPH_NAME,
        gardenRelay: process.env.STORYBOOK_GARDEN_RELAY,
        gardenHubAddress: process.env.STORYBOOK_GARDEN_HUB_ADDRESS,
        gardenProxyFactoryAddress:
          process.env.STORYBOOK_GARDEN_PROXY_FACTORY_ADRESS,
        gardenSafeMasterAddress:
          process.env.STORYBOOK_GARDEN_SAFE_MASTER_ADDRESS,
      }
    : (() => {
        throw new Error();
      })()
) as never;

const OnboardingContent = ({
  initState,
  lang = 'en',
  baseColor,
  content = {},
}: OnboardingProps): ReactElement => {
  const [theme, setColor] = useContext(ThemeContext);
  const [state, act] = useStateMachine(initState || init, control);

  i18n.changeLanguage(lang);

  useEffect(() => {
    if (baseColor) setColor(baseColor);
  }, [baseColor]);

  return (
    <AnimProvider state={state}>
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
    </AnimProvider>
  );
};
