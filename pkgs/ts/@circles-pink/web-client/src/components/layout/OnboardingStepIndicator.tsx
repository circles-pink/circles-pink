import React from 'react';
import tw from 'twin.macro';
import { StepIndicator } from '../StepIndicator';
import { stateToIndex, useAnimContext } from '../../context/anim';
import { useContext } from 'react';
import { ThemeContext } from '../../context/theme';

const StepIndicatorContainer = tw.div`p-4`;

export const OnboardingStepIndicator = () => {
  const [theme] = useContext(ThemeContext);
  const anim = useAnimContext();

  return (
    <StepIndicatorContainer>
      <StepIndicator
        height={24}
        speed={0.0004}
        selected={stateToIndex(anim.selected)}
        prevSelected={stateToIndex(anim.prevSelected)}
        lastAction={anim.lastAction}
        theme={{ active: theme.baseColor, inActive: '#ebebeb' }}
        steps={[
          { label: '1' },
          { label: '2' },
          { label: '3' },
          { label: '4' },
          { label: '5' },
          { label: '6' },
        ]}
      />
    </StepIndicatorContainer>
  );
};
