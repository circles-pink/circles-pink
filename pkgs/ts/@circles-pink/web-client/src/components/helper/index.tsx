import { ReactElement } from 'react';
import tw, { css, styled } from 'twin.macro';

// -----------------------------------------------------------------------------
// Flex
// -----------------------------------------------------------------------------

export const JustifyEnd = tw.div`flex justify-end`;
export const JustifyStart = tw.div`flex justify-start`;
export const JustifyEndCenter = tw.div`flex justify-end items-center`;
export const JustifyStartCenter = tw.div`flex justify-start items-center gap-x-0.5`;
export const JustifyAround = tw.div`flex justify-around`;
export const JustifyBetween = tw.div`flex justify-between`;
export const JustifyAroundCenter = tw.div`flex justify-around items-center gap-x-0.5`;
export const JustifyBetweenCenter = tw.div`flex justify-between items-center`;

// -----------------------------------------------------------------------------
// Text
// -----------------------------------------------------------------------------

export const CenterText = tw.div`text-center`;
export const StatusContainer = tw.div`my-1`;
export const Status = tw.div`text-red-600`;

// -----------------------------------------------------------------------------
// Grid
// -----------------------------------------------------------------------------

export const TwoButtonRow = tw.span`grid grid-cols-2 gap-2`;
export const ButtonRow = tw.span`grid lg:grid-flow-col md:grid-flow-col grid-flow-row gap-2`;
export const TwoButtonCol = tw.span``;

// -----------------------------------------------------------------------------
// Margin
// -----------------------------------------------------------------------------

type MarginProps = {
  top?: number;
  bottom?: number;
  right?: number;
  left?: number;
  children: ReactElement | ReactElement[] | string;
};

export const Margin = styled.div<MarginProps>(
  ({ top = 0, bottom = 0, right = 0, left = 0 }) => [
    css`
      margin-top: ${top}rem;
      margin-bottom: ${bottom}rem;
      margin-right: ${right}rem;
      margin-left: ${left}rem;
    `,
  ]
);

// -----------------------------------------------------------------------------
// Padding
// -----------------------------------------------------------------------------

type PaddingProps = {
  top?: number;
  bottom?: number;
  right?: number;
  left?: number;
  children: ReactElement | ReactElement[] | string;
};

export const Padding = styled.div<PaddingProps>(
  ({ top = 0, bottom = 0, right = 0, left = 0 }) => [
    css`
      padding-top: ${top}rem;
      padding-bottom: ${bottom}rem;
      padding-right: ${right}rem;
      padding-left: ${left}rem;
    `,
  ]
);
