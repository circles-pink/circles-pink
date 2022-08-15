import tw, { css, styled } from 'twin.macro';
import { darken, lighten } from '../../onboarding/utils/colorUtils';
import React, { ReactNode } from 'react';
import { LoadingCircles } from '../LoadingCircles';
import { JustifyAroundCenter } from '../helper';
import Icon from '@mdi/react';
import { defaultTheme, Theme } from '../../context/theme';

// -----------------------------------------------------------------------------
// UI / Button
// -----------------------------------------------------------------------------

export type ButtonPrio = 'high' | 'medium' | 'low';

export type ButtonState = 'disabled' | 'loading' | 'enabled';

type ButtonProps = {
  theme?: Theme;
  fullWidth?: boolean;
  state?: ButtonState;
  children?: ReactNode;
  onClick?: () => void;
  prio?: ButtonPrio;
  icon?: string | null;
};

export const Button = (props_: ButtonProps) => {
  const props = normalizeProps(props_);
  const buttonColor = mapLoadingColor(props.prio, props.theme);

  return (
    <Button_
      disabled={props.state === 'disabled' || props.state === 'loading'}
      {...props}
    >
      <ButtonContent>
        <TextWrapper state={props.state} theme={props.theme}>
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

const Button_ = styled.button<Button_Props>(
  ({ theme, fullWidth, prio, state }) => {
    const isLoading = state === 'loading';
    const isDisabled = state === 'disabled';

    const prioStyles = {
      high: `
      background: ${theme.baseColor};
      border: 1px solid ${darken(theme.baseColor)};
      color: ${theme.textColorLight};
      &:hover {
        background: ${darken(theme.baseColor)};
        background: ${darken(theme.baseColor)};
        color: ${theme.textColorLight};
      }
    `,
      medium: `
      background: ${theme.darkColor};
      border: 1px solid ${theme.darkColor};
      color: ${theme.textColorLight};
      &:hover {
        background: ${lighten(theme.darkColor, 0.2)};
        color: ${theme.textColorLight};
      }
  `,
      low: `
      background: ${theme.lightColor};
      border: 1px solid ${lighten(theme.darkColor)};
      color: ${theme.darkColor};
      &:hover {
        background: ${lighten(theme.lightColor, 0.75)};
        color: ${theme.darkColor};
      }
      `,
    }[prio];

    return [
      css`
        outline: none;
        border: none;
        font-size: 1.1rem;
        cursor: ${isLoading ? 'wait' : isDisabled ? 'not-allowed' : 'pointer'}
          ${fullWidth && 'width: 100%;'};
        ${prioStyles}
      `,
      tw`font-bold py-2 px-4 my-1 rounded-full`,
    ];
  }
);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const ButtonContent = styled.span(() => [tw`flex justify-around items-center`]);

// -----------------------------------------------------------------------------
// UI / TextWrapper
// -----------------------------------------------------------------------------

type TextWrapperProps = { state: ButtonState; theme: Theme };

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
    theme: props.theme || defaultTheme,
    fullWidth: props.fullWidth === undefined ? false : props.fullWidth,
    children: props.children || 'ok',
    onClick: props.onClick || (() => {}),
    prio: props.prio || 'medium',
    icon: props.icon || null,
  };
};

const mapLoadingColor = (prio: ButtonPrio, theme: Theme): string => {
  switch (prio) {
    case 'high':
      return lighten(theme.baseColor);
    case 'medium':
      return theme.textColorLight;
    case 'low':
      return theme.darkColor;
  }
};
