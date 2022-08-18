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
export const ButtonRow = tw.span`grid grid-flow-col gap-2`;

// -----------------------------------------------------------------------------
// Margin
// -----------------------------------------------------------------------------

type MarginYProps = {
  size: number;
  children: ReactElement | ReactElement[] | string;
};

export const MarginY = styled.div<MarginYProps>(({ size, children }) => [
  css`
    margin: ${size}rem 0;
  `,
]);

type MarginXProps = {
  size: number;
  children: ReactElement | ReactElement[] | string;
};

export const MarginX = styled.div<MarginXProps>(({ size, children }) => [
  css`
    margin: 0 ${size}rem;
  `,
]);
