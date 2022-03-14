import tw, { css, styled } from 'twin.macro';

type ClaimProps = {
  color: string;
};
export const Claim = styled.span<ClaimProps>(({ color }) => [
  css`
    color: ${color};
  `,
  tw`mb-2 text-3xl sm:text-4xl font-extrabold tracking-tight block`,
]);
