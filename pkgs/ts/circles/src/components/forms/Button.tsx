import tw, { css, styled } from 'twin.macro';
import chroma from 'chroma-js';
import { darken, lighten } from '../../onboarding/utils/colorUtils';
import { state } from 'fp-ts';
import { ReactElement, ReactNode } from 'react';

type Button_Props = Required<ButtonProps>;

export const Button_ = styled.button<Button_Props>(
  ({ color, fullWidth, light }) => {
    const coloredTheme = `
    background: ${color || '#6e6e6e'};
    color: white;
    &:hover {
      background: ${color ? darken(color) : darken('#6e6e6e')};
    }
    `;

    const lightTheme = `
    background: white;
    border: 1px solid ${color ? color : lighten('#8e8e8e')};
    color: black;
    &:hover {
      background: ${color ? lighten(color) : lighten('#6e6e6e')};
    }
    `;
    return [
      css`
        ${fullWidth && 'width: 100%;'};
        ${light ? lightTheme : coloredTheme}
      `,
      tw`font-bold py-2 px-4 rounded-full mr-1 cursor-pointer`,
    ];
  }
);

// -----------------------------------------------------------------------------
// Button
// -----------------------------------------------------------------------------

type ButtonProps = {
  color?: string;
  fullWidth?: boolean;
  light?: boolean;
  state?: ButtonState;
  children: ReactNode;
};

type ButtonState = 'disabled' | 'loading' | 'enabled';

const Button = ({ children, state }: ButtonProps) => (
  <Button_>
    {children}
    {state === 'loading' ? '...' : ''}
  </Button_>
);
