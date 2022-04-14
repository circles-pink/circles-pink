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
} from './views';

// Style
import '../styles/global.css';
import { ThemeProvider, ThemeContext } from '../context/theme';
import { AnimProvider } from '../context/anim';
import { CirclesAction } from 'generated/output/CirclesPink.Garden.StateMachine.Action';

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

const envRaw = {
  gardenApi: process.env.STORYBOOK_GARDEN_API,
  gardenApiUsers: process.env.STORYBOOK_GARDEN_API_USERS,
  gardenGraphApi: process.env.STORYBOOK_GARDEN_GRAPH_API,
  gardenSubgraphName: process.env.STORYBOOK_GARDEN_SUBGRAPH_NAME,
  gardenRelay: process.env.STORYBOOK_GARDEN_RELAY,
  gardenHubAddress: process.env.STORYBOOK_GARDEN_HUB_ADDRESS,
  gardenProxyFactoryAddress: process.env.STORYBOOK_GARDEN_PROXY_FACTORY_ADRESS,
  gardenSafeMasterAddress: process.env.STORYBOOK_GARDEN_SAFE_MASTER_ADDRESS,
  gardenEthereumNodeWebSocket: process.env.STORYBOOK_GARDEN_ETHEREUM_NODE_WS,
};

type RemoveUndefined<T> = { [key in keyof T]: Exclude<T[key], undefined> };

type Env = RemoveUndefined<typeof envRaw>;

const parseEnv = (env_: typeof envRaw): Env => {
  const xs = Object.entries(env_).forEach(([k, v]) => {
    if (v === undefined) {
      throw new Error(`Env var missing: ${k}`);
    }
  });

  return env_ as Env;
};

const env = parseEnv(envRaw);

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
      return null;
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
