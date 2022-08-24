import React, { ReactElement } from 'react';
import { css, styled } from 'twin.macro';
import { Theme } from '../../context/theme';
import { darken } from '../../onboarding/utils/colorUtils';

type TagButtonProps = {
  children: ReactElement | ReactElement[] | string;
  onClick: () => void;
  fontSize?: number;
  theme: Theme;
};

export const TagButton = (props: TagButtonProps): ReactElement => {
  return <TagButton_ {...props} />;
};

const TagButton_ = styled.button<TagButtonProps>(({ fontSize = 1, theme }) => [
  css`
    background: ${theme.lightColor};
    color: ${theme.baseColor};
    cursor: pointer;
    font-size: ${fontSize}rem;
    padding: 0.25rem;
    margin: 0.25rem;
    border-radius: 5px;
    outline: none;
    border: 1px solid ${theme.baseColor};

    &:hover {
      color: ${darken(theme.baseColor, 0.1)};
      background: ${darken(theme.lightColor, 0.25)};
    }
  `,
]);
