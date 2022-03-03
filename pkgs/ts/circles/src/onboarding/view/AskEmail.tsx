import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';
import { LabeledCheckbox } from '../../components/Checkbox';
import { mapIndicatorColors } from '../mapIndicatorColors';
import { unit } from 'generated/output/Data.Unit';
import { Direction, FadeIn } from '../../components/animation/FadeIn';

type AskEmailProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskEmail = ({ state, act }: AskEmailProps): ReactElement => {
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
            <Claim>How do you want to be notified?</Claim>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <SubClaim>Please submit a valid e-mail address.</SubClaim>
          </FadeIn>
        </Text>
      }
      interaction={
        <>
          <FadeIn direction={direction} delay={getDelay()}>
            <InputWithProps
              autoFocus
              indicatorColor={mapIndicatorColors(state.emailApiResult)}
              type="text"
              value={state.email}
              placeholder={'Enter your E-Mail'}
              onChange={e => act(A._askEmail(A._setEmail(e.target.value)))}
              onKeyPress={e =>
                e.key === 'Enter' && act(A._askEmail(A._next(unit)))
              }
            />
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <div>
              <LabeledCheckbox
                label="Accept terms"
                checked={state.terms}
                setChecked={() => act(A._askEmail(A._setTerms(unit)))}
              />
            </div>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <div>
              <LabeledCheckbox
                label="Accept privacy"
                checked={state.privacy}
                setChecked={() => act(A._askEmail(A._setPrivacy(unit)))}
              />
            </div>
          </FadeIn>
        </>
      }
      debug={<pre>{JSON.stringify(state.emailApiResult, null, 2)}</pre>}
      control={
        <>
          <FadeIn direction={direction} delay={getDelay()}>
            <ButtonGray onClick={() => act(A._askEmail(A._prev(unit)))}>
              Back
            </ButtonGray>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <ButtonPink onClick={() => act(A._askEmail(A._next(unit)))}>
              Next
            </ButtonPink>
          </FadeIn>
        </>
      }
    />
  );
};
