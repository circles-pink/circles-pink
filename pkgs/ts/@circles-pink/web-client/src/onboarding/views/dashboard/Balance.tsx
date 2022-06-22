import { DefaultView } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import React, { useEffect, useState } from 'react';
import tw, { css, styled } from 'twin.macro';
import { CurrencySymbol } from '../../../components/CurrencySymbol';
import { Theme } from '../../../context/theme';
import { displayBalance } from '../../utils/timeCircles';

type BalanceProps = {
  theme: Theme;
  balance: DefaultView['getBalanceResult'];
};

export const Balance = ({ theme, balance }: BalanceProps) => {
  // a state is needed here, to avoid flickering balance display
  const [resBalance, setResBalance] = useState<string>('0');
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const disableLoading = () => {
    // Disable loading after 2 seconds
    setTimeout(() => {
      setIsLoading(false);
    }, 2000);
  };

  useEffect(() => {
    if (balance.type == 'success') {
      setResBalance(balance.value.data.toString());
      disableLoading();
    } else if (balance.type == 'loading') {
      setIsLoading(true);
    } else {
      disableLoading();
    }
  }, [balance]);

  return (
    <>
      <BalanceWrapper>
        <Amount color={theme.baseColor}>{displayBalance(resBalance)}</Amount>
        <CurrencySymbol color={theme.baseColor} isLoading={isLoading} />
      </BalanceWrapper>
    </>
  );

  // switch (balance.type) {
  //   case 'notAsked':
  //   case 'failure':
  //   case 'loading':
  //     return (
  //       <BalanceWrapper>
  //         <Amount color={theme.baseColor}>0.00</Amount>
  //         <CirclesCurrency color={theme.baseColor} />
  //       </BalanceWrapper>
  //     );
  //   case 'success':
  //     return (
  //       <BalanceWrapper>
  //         <Amount color={theme.baseColor}>{displayBalance(resBalance)}</Amount>
  //         <CirclesCurrency color={theme.baseColor} />
  //       </BalanceWrapper>
  //     );
  //   default:
  //     return <></>;
  // }
};

// -----------------------------------------------------------------------------
// UI / Amount
// -----------------------------------------------------------------------------

type AmountProps = { color?: string };

const Amount = styled.h2<AmountProps>(({ color }) => [
  tw`text-5xl mr-2 my-2`,
  css`
    color: ${color || 'black'};
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
