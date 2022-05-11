import tw, { css, styled } from 'twin.macro';
import { darken, lighten } from '../../onboarding/utils/colorUtils';
import React, { ReactNode } from 'react';
import { LoadingCircles } from '../LoadingCircles';
import { JustifyAroundCenter } from '../helper';
import Icon from '@mdi/react';

// -----------------------------------------------------------------------------
// UI / Button
// -----------------------------------------------------------------------------

export type ButtonPrio = 'high' | 'medium' | 'low';

export type ButtonState = 'disabled' | 'loading' | 'enabled';

type ButtonProps = {
  color?: string;
  fullWidth?: boolean;
  state?: ButtonState;
  children?: ReactNode;
  onClick?: () => void;
  prio?: ButtonPrio;
  icon?: string | null;
};

export const Button = (props_: ButtonProps) => {
  const props = normalizeProps(props_);
  const buttonColor = mapLoadingColor(props.prio, props.color);

  return (
    <Button_ {...props}>
      <ButtonContent>
        <TextWrapper state={props.state}>
          {props.icon ? (
            <JustifyAroundCenter>
              <ButtonText>{props.children}</ButtonText>
              <Icon path={props.icon} size={1} color={'white'} />
            </JustifyAroundCenter>
          ) : (
            props.children
          )}
        </TextWrapper>
        {props.state === 'loading' ? <Loading color={buttonColor} /> : ''}
      </ButtonContent>
    </Button_>
  );
};

// -----------------------------------------------------------------------------
// UI / Button_
// -----------------------------------------------------------------------------

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
    tw`font-bold py-2 px-4 my-1 rounded-full cursor-pointer`,
  ];
});

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const ButtonContent = styled.span(() => [tw`flex justify-around items-center`]);

// -----------------------------------------------------------------------------
// UI / TextWrapper
// -----------------------------------------------------------------------------

type TextWrapperProps = { state: ButtonState };

const TextWrapper = styled.span<TextWrapperProps>(props => [
  css`
    transition: margin-right 0.2s;
    margin-right: ${props.state === 'loading' ? '10' : '0'}px;
  `,
]);

// Gap to icon
const ButtonText = tw.span`mr-3`;

// -----------------------------------------------------------------------------
// UI / Loading
// -----------------------------------------------------------------------------

const loadingWidth = 40;

type LoadingProps = {
  color?: string;
};

const Loading = ({ color }: LoadingProps) => {
  return <LoadingCircles width={40} duration={0.75} color={color} />;
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
    icon: props.icon || null,
  };
};

const mapLoadingColor = (prio: ButtonPrio, themeColor: string): string => {
  switch (prio) {
    case 'high':
      return lighten(themeColor);
    case 'medium':
      return lighten(darkColor);
    case 'low':
      return darkColor;
  }
};
