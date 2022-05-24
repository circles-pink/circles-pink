import tw, { css, styled } from 'twin.macro';

export const SubClaim = styled.p(() => [
  css`
    padding-bottom: 0.25rem;
    font-size: 1.15rem;
    font-weight: 600;
  `,
  tw`block text-gray-800`,
]);
