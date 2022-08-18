import * as TrustState from '@circles-pink/state-machine/output/CirclesPink.Data.TrustState';
import { FadeIn, getIncrementor } from 'anima-react';
import { t } from 'i18next';
import React from 'react';
import tw from 'twin.macro';
import { Theme } from '../context/theme';
import { LoadingText } from './text';

type TrustStatusMessageProps = {
  theme: Theme;
  trustState: TrustState.TrustState;
};

export const TrustStatusMessage = ({
  theme,
  trustState,
}: TrustStatusMessageProps) => {
  // animation
  const getDelay = getIncrementor(0, 0.05);

  return (
    <>
      <FadeIn orientation={'left'} delay={getDelay()}>
        <TrustActionMessageContainer>
          <TrustActionMessage>
            <LoadingText theme={theme} fontSize={1.6}>
              {mapStatusMessage(trustState)}
            </LoadingText>
          </TrustActionMessage>
        </TrustActionMessageContainer>
      </FadeIn>
    </>
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const mapStatusMessage = (trustState: TrustState.TrustState) => {
  if (TrustState.isLoadingTrust(trustState)) {
    return t('dashboard.trustList.message.loadingTrust');
  } else if (TrustState.isLoadingUntrust(trustState)) {
    return t('dashboard.trustList.message.loadingUntrust');
  } else if (TrustState.isPendingTrust(trustState)) {
    return t('dashboard.trustList.message.pendingTrust');
  } else if (TrustState.isPendingUntrust(trustState)) {
    return t('dashboard.trustList.message.pendingUntrust');
  }
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const TrustActionMessageContainer = tw.div`relative w-6 h-6`;
const TrustActionMessage = tw.span`absolute right-0 top-0`;