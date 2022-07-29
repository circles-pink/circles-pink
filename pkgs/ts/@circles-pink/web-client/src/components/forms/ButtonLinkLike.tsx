import React, { ReactElement } from 'react';
import { css, styled } from 'twin.macro';

type ButtonLinkLikeProps = {
  children: ReactElement | ReactElement[] | string;
  onClick: () => void;
};

export const ButtonLinkLike = (props: ButtonLinkLikeProps): ReactElement => {
  return <ButtonLinkLike_ {...props} />;
};

const ButtonLinkLike_ = styled.button<ButtonLinkLikeProps>(() => [
  css`
    text-decoration: underline;
    outline: none;
    border: none;
    background: none;
    color: black;
    cursor: pointer;
    font-size: 1rem;

    &:hover {
      color: gray;
    }
  `,
]);
