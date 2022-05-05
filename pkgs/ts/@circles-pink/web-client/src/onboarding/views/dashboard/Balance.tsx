import { TokenGetBalanceResult } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import React from 'react';
import tw, { css, styled } from 'twin.macro';
import { CirclesCurrency } from '../../../assets/CirclesCurrency';
import { Theme } from '../../../context/theme';
import { displayBalance } from '../../utils/timeCircles';

type BalanceProps = {
  theme: Theme;
  balance: TokenGetBalanceResult;
};

export const Balance = ({ theme, balance }: BalanceProps) => {
  switch (balance.type) {
    case 'notAsked':
    case 'failure':
    case 'loading':
      return (
        <BalanceWrapper>
          <Amount color={theme.baseColor}>0.00</Amount>
          <CirclesCurrency color={theme.baseColor} />
        </BalanceWrapper>
      );
    case 'success':
      return (
        <BalanceWrapper>
          <Amount color={theme.baseColor}>
            {displayBalance(balance.value.data.toString())}
          </Amount>
          <CirclesCurrency color={theme.baseColor} />
        </BalanceWrapper>
      );
  }
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

const BalanceWrapper = tw.div`flex flex-row items-center m-2`;
