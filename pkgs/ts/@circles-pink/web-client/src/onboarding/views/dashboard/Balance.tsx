import { DefaultView } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import React, { useEffect, useState } from 'react';
import tw, { css, styled } from 'twin.macro';
import { CurrencySymbol } from '../../../components/CurrencySymbol';
import { Theme } from '../../../context/theme';
import { displayBalance } from '../../utils/timeCircles';

type BalanceProps = {
  theme: Theme;
  balance: DefaultView['getBalanceResult'];
  checkUBIPayoutResult: DefaultView['checkUBIPayoutResult'];
  requestUBIPayoutResult: DefaultView['requestUBIPayoutResult'];
};

export const Balance = ({
  theme,
  balance,
  requestUBIPayoutResult,
}: BalanceProps) => {
  // a state is needed here, to avoid flickering balance display
  const [resBalance, setResBalance] = useState<string>('0');
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [isRequesting, setIsRequesting] = useState<boolean>(false);

  useEffect(() => {
    if (balance.type === 'success') {
      setResBalance(balance.value.data.toString());
      setIsLoading(false);
    } else if (balance.type === 'loading') {
      setIsLoading(true);
    } else {
      setIsLoading(false);
    }
  }, [balance]);

  useEffect(() => {
    // When requesting a UBI payout, we want to indicate
    // that to the user with a rotation animation
    if (requestUBIPayoutResult.type === 'loading') {
      setIsRequesting(true);
    } else {
      setIsRequesting(false);
    }
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
