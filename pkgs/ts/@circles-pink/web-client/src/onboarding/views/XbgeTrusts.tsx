import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import React, {
  ReactElement,
  useContext,
  useEffect,
  useMemo,
  useState,
} from 'react';
import { Button, ButtonLinkLike } from '../../components/forms';
import { Claim, JustText, SubClaim } from '../../components/text';
import { XbgeDialogCard } from '../../components/XbgeDialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { Theme, ThemeContext } from '../../context/theme';
import Icon from '@mdi/react';
import {
  mdiNumeric1CircleOutline,
  mdiNumeric2CircleOutline,
  mdiNumeric3CircleOutline,
  mdiAccountGroup,
  mdiCashFast,
  mdiShareVariantOutline,
} from '@mdi/js';
import tw, { css, styled } from 'twin.macro';
import { InfoCard } from '../../components/InfoCard';
import QrCode from 'react-qrcode-svg';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import {
  TrustState,
  _trusts,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Trusts';
import {
  defaultView,
  DefaultView,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Trusts.Views';
import { addrToString } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { Margin } from '../../components/helper';
import { LightColorFrame } from '../../components/layout';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type FinalizeMethod = 'collectTrusts' | 'fundSafe';

// -----------------------------------------------------------------------------
// Trusts
// -----------------------------------------------------------------------------

type XbgeTrustsProps = {
  state: TrustState;
  act: (ac: A.CirclesAction) => void;
  sharingFeature: ReactElement | null;
};

export const XbgeTrusts = ({
  state: stateRaw,
  act,
  sharingFeature,
}: XbgeTrustsProps): ReactElement => {
  const state = useMemo<DefaultView>(
    () => (defaultView as any)(stateRaw) as DefaultView,
    [stateRaw]
  );

  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  useEffect(() => {
    const safeStatus = setInterval(
      () => act(_trusts(A._getSafeStatus(unit))),
      5000
    );
    return () => clearInterval(safeStatus);
  }, []);

  return (
    <XbgeDialogCard
      mainContent={
        <>
          <FadeIn orientation={'up'} delay={getDelay()}>
            <Margin bottom={1}>
              <LightColorFrame
                theme={theme}
                title={t('trusts.xbgeSpecial.myWalletTitle')}
              >
                <FadeIn orientation={'up'} delay={getDelay()}>
                  <>
                    <SubClaim>
                      {t('trusts.xbgeSpecial.welcomeUser').replace(
                        '{{user}}',
                        stateRaw.user.username
                      )}
                    </SubClaim>
                    <JustText>
                      {t('trusts.xbgeSpecial.welcomeGeneral')}
                    </JustText>
                  </>
                </FadeIn>
              </LightColorFrame>
            </Margin>
          </FadeIn>

          <LightColorFrame
            title={t('trusts.collectTrustsTitle')}
            theme={theme}
            icon={mdiAccountGroup}
          >
            <Margin bottom={2}>
              <FadeIn orientation={orientation} delay={getDelay()}>
                <JustText>
                  {t('trusts.xbgeSpecial.howToActivateWallet')}
                </JustText>
              </FadeIn>
              <FadeIn orientation={orientation} delay={getDelay()}>
                <SubClaim>
                  {
                    t('trusts.xbgeSpecial.howToGetTrusts').split(
                      '{{collectionAction}}'
                    )[0]
                  }

                  <ButtonLinkLike
                    onClick={() =>
                      window.open(
                        'https://www.volksentscheid-grundeinkommen.de/',
                        '_blank'
                      )
                    }
                  >
                    <b>{t('trusts.xbgeSpecial.collectionAction')}</b>
                  </ButtonLinkLike>
                  {
                    t('trusts.xbgeSpecial.howToGetTrusts').split(
                      '{{collectionAction}}'
                    )[1]
                  }
                </SubClaim>
              </FadeIn>
              <FadeIn orientation={orientation} delay={getDelay()}>
                <JustText>
                  {t('trusts.xbgeSpecial.collectSignaturesForVouchers')}
                </JustText>
              </FadeIn>
              <FadeIn orientation={orientation} delay={getDelay()}>
                <JustText>{t('trusts.xbgeSpecial.youWillGetCircles')}</JustText>
              </FadeIn>
            </Margin>

            <TrustIndicatorRow>
              <FadeIn orientation={orientation} delay={getDelay()}>
                <Icon
                  path={mdiNumeric1CircleOutline}
                  size={2}
                  color={state.trusts.length >= 1 ? theme.baseColor : 'gray'}
                />
              </FadeIn>
              <FadeIn orientation={orientation} delay={getDelay()}>
                <Icon
                  path={mdiNumeric2CircleOutline}
                  size={2}
                  color={state.trusts.length >= 2 ? theme.baseColor : 'gray'}
                />
              </FadeIn>
              <FadeIn orientation={orientation} delay={getDelay()}>
                <Icon
                  path={mdiNumeric3CircleOutline}
                  size={2}
                  color={state.trusts.length >= 3 ? theme.baseColor : 'gray'}
                />
              </FadeIn>
            </TrustIndicatorRow>
            <FadeIn orientation={orientation} delay={getDelay()}>
              <>
                {state.isReady ? (
                  <Button
                    theme={theme}
                    fullWidth
                    prio="high"
                    onClick={() =>
                      act(A._trusts(A._finalizeRegisterUser(unit)))
                    }
                    state={mapResults(
                      state.deploySafeResult,
                      state.deployTokenResult
                    )}
                  >
                    {t('finalizeButton')}
                  </Button>
                ) : null}
              </>
            </FadeIn>
          </LightColorFrame>

          {sharingFeature && (
            <Margin top={1}>
              <LightColorFrame
                title={t('trusts.xbgeSpecial.shareFeatureTitle')}
                theme={theme}
                icon={mdiShareVariantOutline}
              >
                {sharingFeature}
              </LightColorFrame>
            </Margin>
          )}
        </>
      }
      debug={<StateMachineDebugger state={state} />}
    />
  );
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const TrustIndicatorRow = tw.div`flex flex-row justify-between mx-16 my-4 gap-4`;

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

type Result = {
  type: 'notAsked' | 'loading' | 'failure' | 'success';
};

const mapResults = (res1: Result, res2: Result) => {
  if (res1.type === 'notAsked' && res2.type === 'notAsked') {
    return 'enabled';
  } else if (res1.type === 'success' && res2.type === 'success') {
    return 'enabled';
  } else if (res1.type === 'failure' || res2.type === 'failure') {
    return 'enabled';
  }
  return 'loading';
};
