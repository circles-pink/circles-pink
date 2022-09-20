import { DashboardState, _RemoteData } from '@circles-pink/state-machine/src';
import BN from 'bn.js';
import { pipe } from 'fp-ts/lib/function';
import React, { useEffect, useState } from 'react';
import tw, { css, styled } from 'twin.macro';
import { CurrencySymbol } from '../../../components/CurrencySymbol';
import { Theme } from '../../../context/theme';
import { displayBalance } from '../../utils/timeCircles';

type BalanceProps = {
  theme: Theme;
  balanceResult: DashboardState['getBalanceResult'];
  requestUBIPayoutResult: DashboardState['requestUBIPayoutResult'];
};

export const Balance = ({
  theme,
  balanceResult,
  requestUBIPayoutResult,
}: BalanceProps) => {
  // a state is needed here, to avoid flickering balance display
  const [resBalance, setResBalance] = useState<BN>(new BN('0'));
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [isRequesting, setIsRequesting] = useState<boolean>(false);

  useEffect(() => {
    pipe(
      balanceResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {
          setIsLoading(false);
        },
        onLoading: () => {
          setIsLoading(true);
        },
        onFailure: () => {
          setIsLoading(false);
        },
        onSuccess: balance => {
          setResBalance(balance.data);
        },
      })
    );
  }, [balanceResult]);

  useEffect(() => {
    // When requesting a UBI payout, we want to indicate
    // that to the user with a rotation animation
    pipe(
      requestUBIPayoutResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {
          setIsRequesting(false);
        },
        onLoading: () => {
          setIsRequesting(true);
        },
        onFailure: () => {
          setIsRequesting(false);
        },
        onSuccess: () => {
          setIsRequesting(false);
        },
      })
    );
  }, [requestUBIPayoutResult]);

  return (
    <>
      <BalanceWrapper>
        <Amount color={theme.baseColor}>
          {displayBalance(resBalance, 'TIME-CIRCLES')}
        </Amount>
        <CurrencySymbol
          color={theme.baseColor}
          isLoading={isLoading}
          isRequesting={isRequesting}
        />
      </BalanceWrapper>
    </>
  );
};

// -----------------------------------------------------------------------------
// UI / Amount
// -----------------------------------------------------------------------------

type AmountProps = { color?: string };

const Amount = styled.span<AmountProps>(({ color }) => [
  tw`text-5xl mr-2 my-2`,
  css`
    color: ${color || 'black'};
    font-weight: 600;
  `,
]);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const BalanceWrapper = styled.div(() => [
  tw`flex flex-row items-center`,
  css`
    margin-top: 0.4rem;
  `,
]);
