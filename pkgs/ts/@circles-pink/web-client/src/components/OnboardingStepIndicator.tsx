import React from 'react';
import tw from 'twin.macro';
import { Step, StepIndicator } from './StepIndicator';
import { useAnimContext } from '../context/anim';
import { useContext } from 'react';
import { ThemeContext } from '../context/theme';
import * as NEA from 'fp-ts/lib/NonEmptyArray';
import { identity, pipe } from 'fp-ts/lib/function';
import * as O from 'fp-ts/lib/Option';
import { CirclesState } from '@circles-pink/state-machine/src';

const StepIndicatorContainer = tw.div`p-4`;

const circlesStates: CirclesState['type'][] = [
  'askUsername',
  'askEmail',
  'infoSecurity',
  'magicWords',
  'submit',
];

type OnboardingStepIndicatorProps = {
  skipStates?: CirclesState['type'][];
};

export const OnboardingStepIndicator = ({
  skipStates,
}: OnboardingStepIndicatorProps) => {
  const [theme] = useContext(ThemeContext);
  const anim = useAnimContext();

  const states = skipStates
    ? circlesStates.filter(state => !skipStates.includes(state))
    : circlesStates;

  const steps: Step[] = states.map(s => {
    return { label: s };
  });

  const nonEmptySteps = pipe(
    steps,
    NEA.fromArray,
    O.match(() => undefined, identity)
  );

  return (
    <StepIndicatorContainer>
      <StepIndicator
        height={24}
        speed={0.0004}
        selected={states.indexOf(anim.selected)}
        prevSelected={states.indexOf(anim.prevSelected)}
        lastAction={anim.lastAction}
        theme={{ active: theme.baseColor, inActive: '#ebebeb' }}
        steps={nonEmptySteps}
      />
    </StepIndicatorContainer>
  );
};
