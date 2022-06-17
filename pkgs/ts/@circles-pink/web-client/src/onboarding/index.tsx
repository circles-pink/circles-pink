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
import * as E from 'fp-ts/Either';
import { Either } from '@circles-pink/state-machine/output/Data.FpTs.Either';

type Language = 'en' | 'de';

type Content = {};

export type OnboardingProps = {
  initState?: CirclesState;
  lang?: Language;
  baseColor?: string;
  content?: Content;
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

const fromFpTsEither = <A, B>(e: E.Either<A, B>): Either<A, B> =>
  e as unknown as Either<string, (_: string) => () => Unit>;

const cfgExample: CirclesConfig = {
  extractEmail: fromFpTsEither(
    E.right((email: string) => () => {
      // Save the email somewhere...
      console.log(email);
      return unit;
    })
  ),
};

const cfg: CirclesConfig = {
  extractEmail: fromFpTsEither(E.left('hello')),
};

const control = mkControl(env)(cfg);

type ViewProps = {
  state: CirclesState;
  act: (m: CirclesAction) => void;
};

const getSkipStates = (): CirclesState['type'][] => {
  const toSkip: CirclesState['type'][] = [];
  if (!cfg.extractEmail) {
    toSkip.push('askEmail');
  }
  return toSkip;
};

const skip = getSkipStates();

const View = ({ state, act }: ViewProps): ReactElement | null => {
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

const OnboardingContent = ({
  initState,
  lang = 'en',
  baseColor,
  content = {},
}: OnboardingProps): ReactElement => {
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
          <View state={state} act={act} />
        </DebugProvider>
      </I18nextProvider>
    </AnimProvider>
  );
};

// -----------------------------------------------------------------------------
// Frame
// -----------------------------------------------------------------------------

const Frame = styled.div(() => [
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
