import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useState } from 'react';
import { ButtonPink } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { Orientation, FadeIn } from '../../components/animation/FadeIn';
import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';

type InfoGeneralProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const InfoGeneral = ({ state, act }: InfoGeneralProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim>Welcome to Circles</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>Let's get you a circles Wallet!</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <ButtonPink onClick={() => act(A._infoGeneral(A._next(unit)))}>
            Next
          </ButtonPink>
        </FadeIn>
      }
    />
  );
};
