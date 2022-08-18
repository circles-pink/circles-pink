import tw, { css, styled } from 'twin.macro';

export const Text = styled.span(() => [tw`text-gray-900 mb-2`]);

export const JustText = styled.p(() => [
  tw`text-gray-900`,
  css`
    margin: 0;
    padding: 0;
  `,
]);
