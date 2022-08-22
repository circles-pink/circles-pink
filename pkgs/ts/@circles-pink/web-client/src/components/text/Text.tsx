import tw, { css, styled } from 'twin.macro';

export const Text = styled.span(() => [tw`text-gray-900 mb-2`]);

type JustTextProps = {
  fontSize?: number;
};

export const JustText = styled.p<JustTextProps>(({ fontSize = 1 }) => [
  tw`text-gray-900`,
  css`
    font-size: ${fontSize}rem;
    margin: 0;
    padding: 0;
    line-height: 1.5;
  `,
]);
