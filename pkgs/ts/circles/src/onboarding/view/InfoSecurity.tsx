import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement } from 'react';
import { ButtonGray, ButtonPink } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';
import { unit } from 'generated/output/Data.Unit';
import { Direction, FadeIn } from '../../components/animation/FadeIn';

type InfoSecurityProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const InfoSecurity = ({
  state,
  act,
}: InfoSecurityProps): ReactElement => {
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
            <Claim>Let's talk about Security</Claim>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <SubClaim>Most important: Keep your seedphrase save!</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn direction={direction} delay={getDelay()}>
          <>
            <ButtonGray onClick={() => act(A._infoSecurity(A._prev(unit)))}>
              Back
            </ButtonGray>

            <ButtonPink onClick={() => act(A._infoSecurity(A._next(unit)))}>
              Next
            </ButtonPink>
          </>
        </FadeIn>
      }
    />
  );
};
