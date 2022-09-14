import React, { ReactElement, useContext, useEffect } from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '../i18n';
import { either } from '@circles-pink/state-machine/output/Data.Either';
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

import { env } from '../env';
import { DebugContext, DebugProvider } from '../context/debug';
import tw, { css, styled } from 'twin.macro';
import * as E from 'fp-ts/Either';
import { XbgeDashboard } from './views/dashboard/XbgeDashboard';
import { XbgeTrusts } from './views/XbgeTrusts';
import { mkI18n } from '../i18n_custom';
import { Resource } from 'i18next';
import { UserConfig } from '../types/user-config';
import {
  CirclesAction,
  CirclesConfigEffect,
  CirclesState,
  Unit,
  unit,
  _Either,
  Either,
  Effect,
  _Maybe,
  TrackingEvent,
  _TrackingEvent,
  Maybe,
  _TrackingResumee,
  Json,
} from '@circles-pink/state-machine/src';
import {
  _circlesAction,
  _dashboardAction,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { Resumee } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingResumee';
import { unsafeUnkownToJson } from '../unsafe-as';
import { pipe } from 'fp-ts/lib/function';

type Language = 'en' | 'de';

type Content = {};

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
  shadowFriends?: Array<string>;
  xbgeSafeAddress?: string;
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

const defaultLeftExtractEmail = _Either.Left('hello@world.de');

type CirclesConfigResolved =
  | { extractEmail: { _tag: 'Left'; left: string } }
  | { extractEmail: { _tag: 'Right'; right: (_: string) => () => Unit } };

const getSkipStates = (cfg: UserConfig): CirclesState['type'][] => {
  const toSkip: CirclesState['type'][] = [];
  if ((cfg as unknown as CirclesConfigResolved).extractEmail._tag === 'Left') {
    toSkip.push('askEmail');
  }
  return toSkip;
};

type ViewProps = {
  state: CirclesState;
  act: (ac: CirclesAction) => void;
  cfg: UserConfig;
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
      if (cfg.xbgeCampaign) {
        return (
          <XbgeTrusts
            state={state.value}
            act={act}
            sharingFeature={cfg.sharingFeature}
          />
        );
      }
      return <Trusts state={state.value} act={act} />;
    case 'dashboard':
      if (cfg.xbgeCampaign) {
        return (
          <XbgeDashboard
            state={state.value}
            act={da => act(_circlesAction._dashboard(da))}
            cfg={cfg}
            sharingFeature={cfg.sharingFeature}
            buyVoucherEurLimit={cfg.buyVoucherEurLimit}
            shadowFriends={cfg.shadowFriends}
            xbgeSafeAddress={cfg.xbgeSafeAddress}
          />
        );
      }
      return (
        <Dashboard
          state={state.value}
          act={da => act(_circlesAction._dashboard(da))}
          cfg={cfg}
          buyVoucherEurLimit={cfg.buyVoucherEurLimit}
        />
      );
    default:
      return null;
  }
};

const mkCfg = (uCfg: UserConfig): CirclesConfigEffect => {
  const defaultRightExtractEmail: Either<string, (_: string) => Effect<Unit>> =
    _Either.Right((email: string) => () => {
      // Save the email somewhere...
      console.log(email);
      return unit;
    });

  const extractEmail: Either<string, (_: string) => Effect<Unit>> =
    typeof uCfg.email === 'string'
      ? _Either.Left(uCfg.email)
      : typeof uCfg.email === 'function'
      ? _Either.Right((email: string) => () => {
          if (uCfg && uCfg.email && typeof uCfg.email !== 'string') {
            uCfg.email(email);
          }
          return unit;
        })
      : defaultRightExtractEmail;

  const onTrackingEvent: Maybe<(_: TrackingEvent) => Effect<Unit>> =
    uCfg.onTrackingEvent
      ? _Maybe.mkMaybe.mkJust(x => () => {
          if (!uCfg?.onTrackingEvent) return;
          return uCfg?.onTrackingEvent(
            _TrackingEvent.encodeJsonTrackingEvent(x)
          );
        })
      : _Maybe.mkMaybe.mkNothing(unit);

  const onTrackingResumee: Maybe<(_: (_: Resumee) => Resumee) => Effect<Unit>> =
    uCfg.onTrackingResumee
      ? _Maybe.mkMaybe.mkJust(f => () => {
          if (!uCfg?.onTrackingResumee) return;
          uCfg.onTrackingResumee((j : unknown) => {
        
             
            if (!j) {

              const resumee_ = f(_TrackingResumee.init);
              const resumee_encoded = _TrackingResumee.encodeJsonResumee(resumee_);
              return resumee_encoded;
            }

            const r = _TrackingResumee.decodeJsonResumee(unsafeUnkownToJson(j));

            pipe(r, _Either.unEither())

            return ()

            return (either as any)(() => {
              throw new Error('Decode error') as any;
            })((ok: Resumee) => {
              const result = f(ok);
              const encodedResult = encodeJsonResumee(result);
              return encodedResult;
            })(r);
          });
        })
      : _Maybe.mkMaybe.mkNothing(unit);

  return {
    extractEmail,
    onTrackingEvent,
    onTrackingResumee,
    // safeAddress,
    // strictMode,
  };
};

// const mkCfg = (uCfg: UserConfig): CirclesConfigEffect => {
//   if (typeof uCfg.email === 'string') {
//     return {
//       extractEmail: _Either.Left(uCfg.email),
//     };
//   }

//   return {
//     extractEmail: fromFpTsEither(
//       E.right((email: string) => () => {
//         if (uCfg && uCfg.email && typeof uCfg.email !== 'string') {
//           uCfg.email(email);
//         }
//         return unit;
//       })
//     ),
//   };
// };

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
  shadowFriends,
  xbgeSafeAddress,
}: OnboardingProps): ReactElement => {
  const userConfig: UserConfig = {
    email,
    onTrackingEvent,
    voucherShopEnabled,
  };

  const cfg_ = mkCfg(userConfig);

  const cfg = {
    ...userConfig,
    ...cfg_,
    onTrackingEvent: onTrackingEvent
      ? Just((x: TrackingEvent) => () => {
          if (!userConfig?.onTrackingEvent) return;
          return userConfig?.onTrackingEvent(encodeJsonTrackingEvent(x));
        })
      : Nothing.value,
    onTrackingResumee: onTrackingResumee
      ? Just((f: (r: Resumee) => Resumee) => () => {
          onTrackingResumee((j : unknown) => {
            if (!j) {
              const resumee_ = f(_TrackingResumee.init);
              const resumee_encoded = _TrackingResumee.encodeJsonResumee(resumee_);
              return resumee_encoded;
            }
            const r = decodeJsonResumee(j as Json);
            return (either as any)(() => {
              throw new Error('Decode error') as any;
            })((ok: Resumee) => {
              const result = f(ok);
              const encodedResult = encodeJsonResumee(result);
              return encodedResult;
            })(r);
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
            shadowFriends={shadowFriends}
            xbgeSafeAddress={xbgeSafeAddress}
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
