import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useState } from 'react';
import { ButtonPink } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';
import { Direction, FadeIn } from '../../components/animation/FadeIn';

type InfoGeneralProps = {
  act: (ac: A.CirclesAction) => void;
};

export const InfoGeneral = ({ act }: InfoGeneralProps): ReactElement => {
  const direction: Direction = 'left';
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
            <Claim>Welcome to Circles</Claim>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <SubClaim>Let's get you a circles Wallet!</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn direction={direction} delay={getDelay()}>
          <ButtonPink onClick={() => act(A._infoGeneral(A._next(unit)))}>
            Next
          </ButtonPink>
        </FadeIn>
      }
    />
  );
};
