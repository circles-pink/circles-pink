import React, { ReactElement, useContext, useEffect } from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '../i18n';
import {
  CirclesConfig,
  mkControl,
  mkControlTestEnv,
  // @ts-ignore
} from '@circles-pink/state-machine/output/CirclesPink.Garden.TS';
import {
  CirclesState,
  init,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import {
  decodeJsonResumee,
  Resumee,
  init as initResumee,
  encodeJsonResumee,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingResumee';
import { useStateMachine } from './useStateMachine';
import {
  AskUsername,
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
import { ThemeProvider, ThemeContext, Theme } from '../context/theme';
import { AnimProvider } from '../context/anim';
import { CirclesAction } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import {
  TrackingEvent,
  encodeJsonTrackingEvent,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingEvent';

import { env } from '../env';
import { DebugContext, DebugProvider } from '../context/debug';
import tw, { css, styled } from 'twin.macro';
import { Unit, unit } from '@circles-pink/state-machine/output/Data.Unit';
import { fromFpTsEither } from '../utils/fpTs';
import * as E from 'fp-ts/Either';
import { Just, Nothing } from '@circles-pink/state-machine/output/Data.Maybe';
import { XbgeDashboard } from './views/dashboard/XbgeDashboard';
import { XbgeTrusts } from './views/XbgeTrusts';
import { Json } from '@circles-pink/state-machine/output/Data.Argonaut.Core';
import { mkI18n } from '../i18n_custom';
import { Resource } from 'i18next';
import { parseAddress } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';

type Language = 'en' | 'de';

type Content = {};

export type UserConfig = {
  email?: string | ((email: string) => void);
  onTrackingEvent?: (json: unknown) => void;
  onTrackingResumee?: (json: unknown) => void;
  voucherShopEnabled: boolean;
};

export type OnboardingProps = {
  initState?: CirclesState;
  lang?: Language;
  theme?: Theme;
  content?: Content;
  email?: string | ((email: string) => void);
  onTrackingEvent?: (json: unknown) => void;
  onTrackingResumee?: (f: (json?: unknown) => unknown) => void;
  voucherShopEnabled?: boolean;
  xbgeCampaign?: boolean;
  testEnv?: Boolean;
  translations?: Resource;
  sharingFeature?: ReactElement | null;
  safeAddress?: string;
  strictMode?: boolean;
  buyVoucherEurLimit?: number;
};

export const Onboarding = (props: OnboardingProps) => {
  if (window === undefined) return null;
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
  cfg: UserConfig;
  xbgeCampaign: boolean;
  sharingFeature: ReactElement | null;
  buyVoucherEurLimit: number;
};

const View = ({
  state,
  act,
  cfg,
  xbgeCampaign,
  sharingFeature,
  buyVoucherEurLimit,
}: ViewProps): ReactElement | null => {
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
      if (xbgeCampaign) {
        return (
          <XbgeTrusts
            state={state.value}
            act={act}
            sharingFeature={sharingFeature}
          />
        );
      }
      return <Trusts state={state.value} act={act} />;
    case 'dashboard':
      if (xbgeCampaign) {
        return (
          <XbgeDashboard
            state={state.value}
            act={act}
            cfg={cfg}
            sharingFeature={sharingFeature}
            buyVoucherEurLimit={buyVoucherEurLimit}
          />
        );
      }
      return (
        <Dashboard
          state={state.value}
          act={act}
          cfg={cfg}
          buyVoucherEurLimit={buyVoucherEurLimit}
        />
      );
    default:
      return null;
  }
};

const mkCfg = (uCfg: UserConfig): CirclesConfig => {
  if (typeof uCfg.email === 'string') {
    return {
      extractEmail: fromFpTsEither(E.left(uCfg.email)),
    };
  }

  return {
    extractEmail: fromFpTsEither(
      E.right((email: string) => () => {
        if (uCfg && uCfg.email && typeof uCfg.email !== 'string') {
          uCfg.email(email);
        }
        return unit;
      })
    ),
  };
};

const OnboardingContent = ({
  initState,
  lang = 'en',
  theme,
  content = {},
  email = () => {},
  onTrackingEvent,
  onTrackingResumee,
  voucherShopEnabled = false,
  xbgeCampaign = false,
  testEnv = false,
  translations,
  sharingFeature,
  safeAddress,
  strictMode = false,
  buyVoucherEurLimit = 70,
}: OnboardingProps): ReactElement => {
  const userConfig: UserConfig = {
    email,
    onTrackingEvent,
    voucherShopEnabled,
  };

  const cfg_ = email ? mkCfg(userConfig) : cfgDefaultRight;

  const cfg = {
    ...userConfig,
    ...cfg_,
    onTrackingEvent: onTrackingEvent
      ? Just.create((x: TrackingEvent) => () => {
          if (!userConfig?.onTrackingEvent) return;
          return userConfig?.onTrackingEvent(encodeJsonTrackingEvent(x));
        })
      : Nothing.value,
    onTrackingResumee: onTrackingResumee
      ? Just.create((f: (r: Resumee) => Resumee) => () => {
          onTrackingResumee(j => {
            if (!j) return encodeJsonResumee(f(initResumee));
            const r = decodeJsonResumee(j as Json);
            switch (r.constructor.name) {
              case 'Right':
                return encodeJsonResumee(f(r.value0 as Resumee));
              case 'Left':
                throw new Error('Decode error');
            }
          });
        })
      : Nothing.value,
    safeAddress: safeAddress ? parseAddress(safeAddress) : Nothing.value,
    strictMode,
  };

  const control = testEnv ? mkControlTestEnv : mkControl(env)(cfg);

  const [state, act] = (useStateMachine as any)(
    (initState as unknown as CirclesState) || (init as unknown as CirclesState),
    control
  );
  const [_theme, setTheme] = useContext(ThemeContext);

  useEffect(() => {
    if (theme) {
      const mergedTheme = { ..._theme, ...theme };
      setTheme(mergedTheme);
    }
  }, [theme]);

  const customI18n = translations ? mkI18n(translations) : i18n;

  customI18n.changeLanguage(lang);

  return (
    <AnimProvider state={state}>
      <I18nextProvider i18n={customI18n}>
        <DebugProvider>
          <View
            state={state}
            act={act}
            cfg={cfg}
            xbgeCampaign={xbgeCampaign}
            sharingFeature={sharingFeature || null}
            buyVoucherEurLimit={buyVoucherEurLimit}
          />
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
