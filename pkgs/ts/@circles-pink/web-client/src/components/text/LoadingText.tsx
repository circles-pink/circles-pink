import styled from '@emotion/styled';
import React, { ReactElement } from 'react';
import { css } from 'twin.macro';
import { Theme } from '../../context/theme';

type LoadingTextProps = {
  children: string;
  theme: Theme;
  fontSize: number;
};

export const LoadingText = (props: LoadingTextProps): ReactElement => {
  return <ShimmeringText {...props}>{props.children}</ShimmeringText>;
};

type ShimmeringTextProps = LoadingTextProps

const ShimmeringText = styled.p<ShimmeringTextProps>(({theme, fontSize}) => [
  css`
    padding: 0;
    margin: 0;
    font-size: ${fontSize}rem;
    color: rgba(255, 255, 255, 0.1);
    background: -webkit-gradient(linear, left top, right top, from(${theme.baseColor}), to(${theme.baseColor}), color-stop(0.5, ${theme.textColorLight}));
    background: -moz-gradient(linear, left top, right top, from(${theme.baseColor}), to(${theme.baseColor}), color-stop(0.5, ${theme.textColorLight}));
    background: gradient(linear, left top, right top, from(${theme.baseColor}), to(${theme.baseColor}), color-stop(0.5, ${theme.textColorLight}));
    -webkit-background-size: 125px 100%;
    -moz-background-size: 125px 100%;
    background-size: 125px 100%;
    -webkit-background-clip: text;
    -moz-background-clip: text;
    background-clip: text;
    -webkit-animation-name: shimmer;
    -moz-animation-name: shimmer;
    animation-name: shimmer;
    -webkit-animation-duration: 3s;
    -moz-animation-duration: 3s;
    animation-duration: 3s;
    -webkit-animation-iteration-count: infinite;
    -moz-animation-iteration-count: infinite;
    animation-iteration-count: infinite;
    background-repeat: no-repeat;
    background-position: 0 0;
    background-color: ${theme.baseColor};

    @-moz-keyframes shimmer {
      0% {
          background-position: top left;
      }
      100% {
          background-position: top right;
      }
    }
    
    @-webkit-keyframes shimmer {
        0% {
            background-position: top left;
        }
        100% {
            background-position: top right;
        }
    }
    
    @-o-keyframes shimmer {
        0% {
            background-position: top left;
        }
        100% {
            background-position: top right;
        }
    }
    
    @keyframes shimmer {
        0% {
            background-position: top left;
        }
        100% {
            background-position: top right;
        }
    }
  `,
]);
