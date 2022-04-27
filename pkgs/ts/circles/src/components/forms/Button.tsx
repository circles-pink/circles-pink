import tw, { css, styled } from 'twin.macro';
import { darken, lighten } from '../../onboarding/utils/colorUtils';
import React, { ReactNode } from 'react';

type Button_Props = Required<ButtonProps>;

const Button_ = styled.button<Button_Props>(({ color, fullWidth, light }) => {
  const coloredTheme = `
    background: ${color};
    color: white;
    &:hover {
      background: ${darken(color)};
    }
    `;

  const lightTheme = `
    background: white;
    border: 1px solid ${lighten('#8e8e8e')};
    color: black;
    &:hover {
      background: ${lighten('#6e6e6e')};
    }
    `;
  return [
    css`
      ${fullWidth && 'width: 100%;'};
      ${light ? lightTheme : coloredTheme}
    `,
    tw`font-bold py-2 px-4 rounded-full mr-1 cursor-pointer`,
  ];
});

// -----------------------------------------------------------------------------
// Button
// -----------------------------------------------------------------------------

type ButtonProps = {
  color?: string;
  fullWidth?: boolean;
  light?: boolean;
  state?: ButtonState;
  children?: ReactNode;
  onClick?: () => void;
};

export type ButtonState = 'disabled' | 'loading' | 'enabled';

export const Button = (props_: ButtonProps) => {
  const props = normalizeProps(props_);
  return (
    <Button_ {...props}>
      {props.children}
      {props.state === 'loading' ? '...' : ''}
    </Button_>
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const normalizeProps = (props: ButtonProps): Required<ButtonProps> => {
  console.log(props);

  return {
    state: props.state || 'enabled',
    color: props.color || '#6e6e6e',
    fullWidth: props.fullWidth === undefined ? false : props.fullWidth,
    light: props.light || false,
    children: props.children || null,
    onClick: props.onClick || (() => {}),
  };
};
