import React, { ReactElement } from 'react';
import { css, styled } from 'twin.macro';

type ButtonLinkLikeProps = {
  children: ReactElement | ReactElement[] | string;
  onClick: () => void;
  fontSize?: number;
};

export const ButtonLinkLike = (props: ButtonLinkLikeProps): ReactElement => {
  return <ButtonLinkLike_ {...props} />;
};

const ButtonLinkLike_ = styled.button<ButtonLinkLikeProps>(
  ({ fontSize = 1 }) => [
    css`
      text-decoration: underline;
      outline: none;
      border: none;
      background: none;
      color: black;
      cursor: pointer;
      font-size: ${fontSize}rem;
      padding: 0;

      &:hover {
        color: gray;
      }
    `,
  ]
);
