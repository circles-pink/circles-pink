import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useContext } from 'react';
import { Button } from '../../components/forms';
import { Claim, SubClaim } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { TrustState } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import Icon from '@mdi/react';
import {
  mdiNumeric1CircleOutline,
  mdiNumeric2CircleOutline,
  mdiNumeric3CircleOutline,
  mdiAccountGroup,
  mdiCashFast,
} from '@mdi/js';
import tw, { css, styled } from 'twin.macro';
import { InfoCard } from '../../components/InfoCard';
import QrCode from 'react-qrcode-svg';
import { mapResult } from '../utils/mapResult';

// -----------------------------------------------------------------------------
// Trusts
// -----------------------------------------------------------------------------

type TrustsProps = {
  state: TrustState;
  act: (ac: A.CirclesAction) => void;
};

export const Trusts = ({ state, act }: TrustsProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          {state.user.username ? (
            <Claim color={theme.baseColor}>
              {t('trusts.greet')}
              {` ${state.user.username}!`}
            </Claim>
          ) : (
            <Claim color={theme.baseColor}>{t('trusts.claim')}</Claim>
          )}

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('trusts.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            {/* <Button
              color={theme.baseColor}
              onClick={() => act(A._trusts(A._getSafeStatus(unit)))}
            >
              {t('safeStateButton')}
            </Button> */}
            {state.isReady ? (
              <Button
                color={theme.baseColor}
                onClick={() => act(A._trusts(A._finalizeRegisterUser(unit)))}
                state={mapResult(state.trustsResult)}
              >
                {t('finalizeButton')}
              </Button>
            ) : null}
          </>
        </FadeIn>
      }
      mainContent={
        <FlexRow>
          <InfoCard
            title={'Trusts'}
            text={
              <>
                <FadeIn orientation={orientation} delay={getDelay()}>
                  <Text>{t('trusts.getMoreTruts')}</Text>
                </FadeIn>
                <FadeIn orientation={orientation} delay={getDelay()}>
                  <CenterElement>
                    <QrCode
                      data={state.user.safeAddress}
                      height="200"
                      width="200"
                      fgColor="gray"
                      bgColor="white"
                    />
                  </CenterElement>
                </FadeIn>
                <TrustIndicatorRow>
                  <FadeIn orientation={orientation} delay={getDelay()}>
                    <Icon
                      path={mdiNumeric1CircleOutline}
                      size={2}
                      color={
                        state.trusts.length >= 1 ? theme.baseColor : 'gray'
                      }
                    />
                  </FadeIn>
                  <FadeIn orientation={orientation} delay={getDelay()}>
                    <Icon
                      path={mdiNumeric2CircleOutline}
                      size={2}
                      color={
                        state.trusts.length >= 2 ? theme.baseColor : 'gray'
                      }
                    />
                  </FadeIn>
                  <FadeIn orientation={orientation} delay={getDelay()}>
                    <Icon
                      path={mdiNumeric3CircleOutline}
                      size={2}
                      color={
                        state.trusts.length >= 3 ? theme.baseColor : 'gray'
                      }
                    />
                  </FadeIn>
                </TrustIndicatorRow>
              </>
            }
            themeColor={theme.baseColor}
            icon={mdiAccountGroup}
          />

          <InfoCard
            title={'Gnosis Safe'}
            text={
              <>
                <FadeIn orientation={orientation} delay={getDelay()}>
                  <Text>{t('trusts.fundYourSafe')}</Text>
                </FadeIn>
                <FadeIn orientation={orientation} delay={getDelay()}>
                  <CenterElement>
                    <QrCode
                      data={state.user.safeAddress}
                      height="200"
                      width="200"
                      fgColor="gray"
                      bgColor="white"
                    />
                  </CenterElement>
                </FadeIn>
                <FadeIn orientation={orientation} delay={getDelay()}>
                  <CenterText>
                    <Text>{state.user.safeAddress}</Text>
                  </CenterText>
                </FadeIn>
              </>
            }
            themeColor={theme.baseColor}
            icon={mdiCashFast}
          />
        </FlexRow>
      }
      debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};

// -----------------------------------------------------------------------------
// FlexItemGrow
// -----------------------------------------------------------------------------

const FlexItemGrow = styled.div(() => [
  tw`h-full`,
  css`
    flex-grow: 1;
    flex-basis: 0;
  `,
]);
// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const FlexRow = tw.div`flex lg:flex-row flex-col justify-between mb-4 gap-4`;
const TrustIndicatorRow = tw.div`flex flex-row justify-between mx-16 mb-4 gap-4`;
const CenterText = tw.div`text-center`;
const CenterElement = tw.div`flex justify-around`;
const Text = tw.p`mt-4 text-lg font-medium text-gray-500`;
