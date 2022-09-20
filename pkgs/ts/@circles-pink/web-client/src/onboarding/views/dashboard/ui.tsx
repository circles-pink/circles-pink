// -----------------------------------------------------------------------------
// UI / UserHandle
// -----------------------------------------------------------------------------

import tw, { css, styled } from 'twin.macro';

type UserHandleProps = { color?: string };

export const UserHandle = styled.span<UserHandleProps>(({ color }) => [
  tw`flex justify-around text-lg`,
  css`
    margin: 0;
    padding: 0;
    font-weight: 600;
    color: ${color || 'black'};
  `,
]);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

export const HeaderContent = tw.div`flex justify-around items-center mx-4`;
export const ControlContent = tw.div`lg:my-2 md:my-4`;
export const MainContent = tw.div`grid lg:grid-cols-2 gap-4`;
export const TopMargin = tw.div`mt-4`;
export const TwoCols = tw.div`max-w-7xl grid lg:grid-cols-2 md:grid-cols-2 lg:gap-4 md:gap-4`;
