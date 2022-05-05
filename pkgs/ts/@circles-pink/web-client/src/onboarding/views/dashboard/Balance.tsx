import { TokenGetBalanceResult } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import React from 'react';
import tw, { css, styled } from 'twin.macro';
import { CirclesCurrency } from '../../../assets/CirclesCurrency';
import { Theme } from '../../../context/theme';
import { mapBalanceToHr } from '../../utils/balance';

type BalanceProps = {
  theme: Theme;
  balance: TokenGetBalanceResult;
};
export const Balance = ({ theme, balance }: BalanceProps) => {
  switch (balance.type) {
    case 'notAsked':
      return (
        <BalanceWrapper>
          <Amount color={theme.baseColor}>0.00</Amount>
          <CirclesCurrency color={theme.baseColor} />
        </BalanceWrapper>
      );
    case 'failure':
      return (
        <BalanceWrapper>
          <Amount color={theme.baseColor}>0.00</Amount>
          <CirclesCurrency color={theme.baseColor} />
        </BalanceWrapper>
      );
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
            {mapBalanceToHr(balance.value.data)}
          </Amount>
          <CirclesCurrency color={theme.baseColor} />
        </BalanceWrapper>
      );
  }
};

// -----------------------------------------------------------------------------
// UI / Balance
// -----------------------------------------------------------------------------

type AmountProps = { color?: string };

const Amount = styled.h2<AmountProps>(({ color }) => [
  tw`text-5xl mr-2 my-2`,
  css`
    color: ${color || 'black'};
  `,
]);
const BalanceWrapper = tw.div`flex flex-row items-center m-2`;
