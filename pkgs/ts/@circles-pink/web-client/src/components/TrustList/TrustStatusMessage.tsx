import {
  _TrustState,
  TrustStateType,
  DashboardState,
  UserIdent,
  _UserIdent,
  _Address,
  RemoteData,
  _RemoteData,
} from '@circles-pink/state-machine/src';
import { FadeIn, getIncrementor } from 'anima-react';
import { pipe } from 'fp-ts/lib/function';
import { t } from 'i18next';
import React, { useEffect, useState } from 'react';
import tw from 'twin.macro';
import { Theme } from '../../context/theme';
import { useWindowDimensions } from '../../hooks/useWindowDimensions';
import { toNativeRecord } from '../../safe-as';
import { LoadingText } from '../text';
import { useTrustResult } from './hooks/useTrustResult';

type TrustStatusMessageProps = {
  theme: Theme;
  trustState: TrustStateType;
  userIdent: UserIdent;
  trustAddResult: DashboardState['trustAddResult'];
  trustRemoveResult: DashboardState['trustRemoveResult'];
};

export const TrustStatusMessage = ({
  theme,
  trustState,
  userIdent,
  trustAddResult,
  trustRemoveResult,
}: TrustStatusMessageProps) => {
  // animation
  const getDelay = getIncrementor(0, 0.05);
  const { width } = useWindowDimensions();

  const address = _Address.addrToString(_UserIdent.getAddress(userIdent));

  const trustResult = useTrustResult(
    address,
    trustAddResult,
    trustRemoveResult
  );

  const retry =
    trustResult && trustResult.retry > 0 ? `(${trustResult.retry}) ` : '';

  const statusMsg =
    trustResult?.lastState !== 'failed'
      ? mapStatusMessage(trustState)
      : t('dashboard.trustList.message.trustingFailed');

  return (
    <>
      <FadeIn orientation={'left'} delay={getDelay()}>
        <TrustActionMessageContainer>
          <TrustActionMessage>
            <LoadingText theme={theme} fontSize={width > 767 ? 1.6 : 1.3}>
              {retry + statusMsg}
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

const mapStatusMessage = (trustState: TrustStateType) => {
  if (_TrustState.isLoadingTrust(trustState)) {
    return t('dashboard.trustList.message.loadingTrust');
  } else if (_TrustState.isLoadingUntrust(trustState)) {
    return t('dashboard.trustList.message.loadingUntrust');
  } else if (_TrustState.isPendingTrust(trustState)) {
    return t('dashboard.trustList.message.pendingTrust');
  } else if (_TrustState.isPendingUntrust(trustState)) {
    return t('dashboard.trustList.message.pendingUntrust');
  }
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const TrustActionMessageContainer = tw.div`relative w-6 h-6`;
const TrustActionMessage = tw.span`absolute right-0 top-0`;
