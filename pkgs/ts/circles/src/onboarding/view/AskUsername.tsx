import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';
import { mapIndicatorColors } from '../mapIndicatorColors';
import { unit } from 'generated/output/Data.Unit';
import { Direction, FadeIn } from '../../components/animation/FadeIn';

type AskUsernameProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskUsername = ({ state, act }: AskUsernameProps): ReactElement => {
  const direction: Direction = state.animation as Direction;
  const incrementBy = 0.05;
  const getDelay = (
    n => () =>
      (n += incrementBy)
  )(0 - incrementBy);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn direction={direction} delay={getDelay()}>
            <Claim>Tell me your Name!</Claim>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <SubClaim>Choose wisely, you can not change it later!</SubClaim>
          </FadeIn>
        </Text>
      }
      interaction={
        <FadeIn direction={direction} delay={getDelay()}>
          <InputWithProps
            autoFocus
            indicatorColor={mapIndicatorColors(state.usernameApiResult)}
            type="text"
            value={state.username}
            placeholder={'Your amazing username'}
            onChange={e => act(A._askUsername(A._setUsername(e.target.value)))}
            onKeyPress={e =>
              e.key === 'Enter' && act(A._askUsername(A._next(unit)))
            }
          />
        </FadeIn>
      }
      debug={<pre>{JSON.stringify(state.usernameApiResult, null, 2)}</pre>}
      control={
        <FadeIn direction={direction} delay={getDelay()}>
          <>
            <ButtonGray onClick={() => act(A._askUsername(A._prev(unit)))}>
              Back
            </ButtonGray>

            <ButtonPink onClick={() => act(A._askUsername(A._next(unit)))}>
              Next
            </ButtonPink>
          </>
        </FadeIn>
      }
    />
  );
};
