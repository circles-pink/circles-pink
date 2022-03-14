import tw, { css, styled } from 'twin.macro';
import chroma from 'chroma-js';
import { darken, lighten } from '../../onboarding/utils/colorUtils';

type ButtonProps = {
  color?: string;
  fullWidth?: boolean;
  light?: true;
};

export const Button = styled.button<ButtonProps>(props => {
  const coloredTheme = `
    background: ${props.color || '#6e6e6e'};
    color: white;
    &:hover {
      background: ${props.color ? darken(props.color) : darken('#6e6e6e')};
    }
    `;

  const lightTheme = `
    background: white;
    border: 1px solid ${props.color ? props.color : lighten('#8e8e8e')};
    color: black;
    &:hover {
      background: ${props.color ? lighten(props.color) : lighten('#6e6e6e')};
    }
    `;
  return [
    css`
      ${props.fullWidth && 'width: 100%;'};
      ${props.light ? lightTheme : coloredTheme}
    `,
    tw`font-bold py-2 px-4 rounded-full mr-1 cursor-pointer`,
  ];
});
