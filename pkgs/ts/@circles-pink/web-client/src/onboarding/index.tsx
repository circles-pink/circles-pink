import React, { ReactElement, useContext, useEffect } from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '../i18n';
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
import { XbgeDashboard } from './views/dashboard/XbgeDashboard';
import { XbgeTrusts } from './views/XbgeTrusts';
import { mkI18n } from '../i18n_custom';
import { Resource } from 'i18next';
import { Content, Language, UserConfig } from '../types/user-config';
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
  Address,
  _Address,
  _StateMachine,
  _TS,
} from '@circles-pink/state-machine/src';
import {
  _circlesAction,
  _dashboardAction,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { Resumee } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingResumee';
import { unsafeUnkownToJson } from '../unsafe-as';
import { pipe } from 'fp-ts/lib/function';
import { fromFetchImplNative } from '../safe-as';

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

const getSkipStates = (cfg: UserConfig): CirclesState['type'][] => {
  const toSkip: CirclesState['type'][] = [];
  _Either.unEither({
    onLeft: () => {
      toSkip.push('askEmail');
    },
    onRight: () => {},
  });
  return toSkip;
};

type ViewProps = {
  state: CirclesState;
  act: (ac: CirclesAction) => void;
  cfg: UserConfig;
};

const View = ({ state, act, cfg: _cfg }: ViewProps): ReactElement | null => {
  const cfg = mkCfgWithDefaults(_cfg);

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
            xbgeSafeAddress={cfg.xbgeSafeAddress || undefined}
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

export const mkCfg = (uCfg: UserConfig): CirclesConfigEffect => {
  const defaultRightExtractEmail: Either<string, (_: string) => Effect<Unit>> =
    _Either.mkEither.mkRight((email: string) => () => {
      // Save the email somewhere...
      console.log(email);
      return unit;
    });

  const extractEmail: Either<string, (_: string) => Effect<Unit>> =
    typeof uCfg.email === 'string'
      ? _Either.mkEither.mkLeft(uCfg.email)
      : typeof uCfg.email === 'function'
      ? _Either.mkEither.mkRight((email: string) => () => {
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
          uCfg.onTrackingResumee((j: unknown) => {
            if (!j) {
              const resumee_ = f(_TrackingResumee.init);
              const resumee_encoded =
                _TrackingResumee.encodeJsonResumee(resumee_);
              return resumee_encoded;
            }

            const r = _TrackingResumee.decodeJsonResumee(unsafeUnkownToJson(j));

            return pipe(
              r,
              _Either.unEither({
                onLeft: () => {
                  throw new Error('Decode error');
                },
                onRight: ok => pipe(ok, f, _TrackingResumee.encodeJsonResumee),
              })
            );
          });
        })
      : _Maybe.mkMaybe.mkNothing(unit);

  const safeAddress: Maybe<Address> =
    typeof uCfg.safeAddress === 'string'
      ? _Address.parseAddress(uCfg.safeAddress)
      : _Maybe.mkMaybe.mkNothing(unit);

  return {
    extractEmail,
    onTrackingEvent,
    onTrackingResumee,
    safeAddress,
    strictMode: !!uCfg.strictMode,
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
  sharingFeature = null,
  safeAddress,
  strictMode = false,
  buyVoucherEurLimit = 70,
  shadowFriends,
  xbgeSafeAddress,
}: OnboardingProps): ReactElement => {
  const userProps = {
    email,
    onTrackingEvent,
    voucherShopEnabled,
    safeAddress,
    strictMode,
    xbgeCampaign,
    sharingFeature,
    buyVoucherEurLimit,
    shadowFriends,
    xbgeSafeAddress,
  };
  const cfg = mkCfg(userProps);

  const control = testEnv
    ? _TS.mkControlTestEnv
    : _TS.mkControl(fromFetchImplNative(window.fetch))(env)(cfg);

  const [state, act] = (useStateMachine as any)(
    (initState as unknown as CirclesState) || _StateMachine.initUserData,
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
          <View state={state} act={act} cfg={userProps} />
        </DebugProvider>
      </I18nextProvider>
    </AnimProvider>
  );
};

export const mkCfgWithDefaults = (cfg: UserConfig): Required<UserConfig> => {
  return {
    lang: 'en',
    content: {},
    email: () => {},
    onTrackingEvent: () => {},
    onTrackingResumee: () => {},
    voucherShopEnabled: false,
    xbgeCampaign: false,
    testEnv: false,
    sharingFeature: null,
    strictMode: false,
    buyVoucherEurLimit: 70,
    shadowFriends: [],
    safeAddress: null,
    xbgeSafeAddress: null,
    ...cfg,
  };
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
