import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement } from 'react';
import { DialogCard } from '../../components/DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';
import { mapIndicatorColors } from '../utils/mapIndicatorColors';
import { unit } from 'generated/output/Data.Unit';
import { Orientation, FadeIn } from '../../components/animation/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';

type AskUsernameProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskUsername = ({ state, act }: AskUsernameProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim>Tell me your Name!</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>Choose wisely, you can not change it later!</SubClaim>
          </FadeIn>
        </Text>
      }
      interaction={
        <FadeIn orientation={orientation} delay={getDelay()}>
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
        <FadeIn orientation={orientation} delay={getDelay()}>
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
