import React, { ReactElement, useContext, useEffect } from 'react';
import { Button, ButtonLinkLike } from '../../components/forms';
import { JustText, SubClaim } from '../../components/text';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import Icon from '@mdi/react';
import {
  mdiNumeric1CircleOutline,
  mdiNumeric2CircleOutline,
  mdiNumeric3CircleOutline,
  mdiAccountGroup,
  mdiWalletOutline,
} from '@mdi/js';
import tw from 'twin.macro';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import { Margin } from '../../components/helper';
import {
  CirclesAction,
  RemoteData,
  TrustState,
  unit,
  _Address,
  _EthAddress,
  _RemoteData,
  _StateMachine,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { ButtonState } from '../../components/forms/Button';
import { XbgeDialogCard } from '../../components/XbgeDialogCard';
import { LightColorFrame } from '../../components/layout';
import { SubClaimLike } from '../../components/text/SubClaim';
// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type FinalizeMethod = 'collectTrusts' | 'fundSafe';

// -----------------------------------------------------------------------------
// Trusts
// -----------------------------------------------------------------------------

const { _circlesAction, _trustsAction } = _StateMachine;

type XbgeTrustsProps = {
  state: TrustState;
  act: (ac: CirclesAction) => void;
  sharingFeature: ReactElement | null;
};

export const XbgeTrusts = ({
  state,
  act,
  sharingFeature,
}: XbgeTrustsProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  useEffect(() => {
    const safeStatus = setInterval(
      () => act(_circlesAction._trusts(_trustsAction._getSafeStatus(unit))),
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
                icon={mdiWalletOutline}
              >
                <FadeIn orientation={'up'} delay={getDelay()}>
                  <>
                    <SubClaim>
                      {t('trusts.xbgeSpecial.welcomeUser').replace(
                        '{{user}}',
                        state.user.username
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

              <>
                {sharingFeature && (
                  <Margin top={1} bottom={1}>
                    <FadeIn orientation={orientation} delay={getDelay()}>
                      {sharingFeature}
                    </FadeIn>
                  </Margin>
                )}
              </>

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
                        t('trusts.xbgeSpecial.collectionActionLink'),
                        '_blank'
                      )
                    }
                  >
                    <SubClaimLike>
                      {t('trusts.xbgeSpecial.collectionAction')}
                    </SubClaimLike>
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
                <Margin top={1}>
                  <JustText>
                    {t('trusts.xbgeSpecial.youWillGetCircles')}
                  </JustText>
                </Margin>
              </FadeIn>

              <FadeIn orientation={orientation} delay={getDelay()}>
                <Margin top={1}>
                  <SubClaim>
                    {
                      t('trusts.xbgeSpecial.getHelp').split(
                        '{{telegramOnboardingGroup}}'
                      )[0]
                    }
                    <ButtonLinkLike
                      onClick={() =>
                        window.open(
                          t('trusts.xbgeSpecial.telegramOnboardingGroupLink'),
                          '_blank'
                        )
                      }
                    >
                      <SubClaimLike>
                        {t('trusts.xbgeSpecial.telegramOnboardingGroup')}
                      </SubClaimLike>
                    </ButtonLinkLike>
                    {
                      t('trusts.xbgeSpecial.getHelp').split(
                        '{{telegramOnboardingGroup}}'
                      )[1]
                    }
                  </SubClaim>
                </Margin>
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
                      act(
                        _circlesAction._trusts(
                          _trustsAction._finalizeRegisterUser(unit)
                        )
                      )
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

type ResultStates = 'notAsked' | 'loading' | 'failure' | 'success';

export const mapResults = (
  res1: RemoteData<unknown, unknown, unknown, unknown>,
  res2: RemoteData<unknown, unknown, unknown, unknown>
): ButtonState => {
  const result1 = pipe(
    res1,
    _RemoteData.unRemoteData<unknown, unknown, unknown, unknown, ResultStates>({
      onNotAsked: () => 'notAsked',
      onFailure: () => 'failure',
      onLoading: () => 'loading',
      onSuccess: () => 'success',
    })
  );
  const result2 = pipe(
    res2,
    _RemoteData.unRemoteData<unknown, unknown, unknown, unknown, ResultStates>({
      onNotAsked: () => 'notAsked',
      onFailure: () => 'failure',
      onLoading: () => 'loading',
      onSuccess: () => 'success',
    })
  );
  if (result1 === 'notAsked' && result2 === 'notAsked') {
    return 'enabled';
  } else if (result1 === 'success' && result2 === 'success') {
    return 'enabled';
  } else if (result1 === 'failure' || result2 === 'failure') {
    return 'enabled';
  }
  return 'loading';
};
