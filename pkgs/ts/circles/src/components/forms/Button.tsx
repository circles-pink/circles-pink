import tw, { css, styled } from 'twin.macro';
import { darken, lighten } from '../../onboarding/utils/colorUtils';
import React, { ReactNode } from 'react';

type Button_Props = Required<ButtonProps>;

const lightColor = 'white';
const darkColor = 'black';

const lightGray = '#6e6e6e';
const darkGray = '#8e8e8e';

const Button_ = styled.button<Button_Props>(({ color, fullWidth, prio }) => {
  const prioStyles = {
    high: `
      background: ${color};
      border: 1px solid ${darken(color)};
      color: white;
      &:hover {
        background: ${darken(color)};
      }
    `,
    medium: `
      background: ${lightGray};
      border: 1px solid ${lightGray};
      color: ${lightColor};
      &:hover {
        background: ${darken(lightGray)};
      }
  `,
    low: `
      background: ${lightColor};
      border: 1px solid ${lighten(darkGray)};
      color:  ${darkColor};
      &:hover {
        background: ${lighten('#6e6e6e')};
      }
      `,
  }[prio];

  return [
    css`
      ${fullWidth && 'width: 100%;'};
      ${prioStyles}
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
  state?: ButtonState;
  children?: ReactNode;
  onClick?: () => void;
  prio?: ButtonPrio;
};

export type ButtonPrio = 'high' | 'medium' | 'low';

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
  return {
    state: props.state || 'enabled',
    color: props.color || 'lime',
    fullWidth: props.fullWidth === undefined ? false : props.fullWidth,
    children: props.children || 'ok',
    onClick: props.onClick || (() => {}),
    prio: props.prio || 'medium',
  };
};
