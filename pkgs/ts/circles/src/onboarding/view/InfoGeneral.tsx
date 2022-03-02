import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement } from 'react';
import { ButtonPink } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';

type InfoGeneralProps = {
  act: (ac: A.CirclesAction) => void;
};

export const InfoGeneral = ({ act }: InfoGeneralProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>Welcome to Circles</Claim>
          <SubClaim>Let's get you a circles Wallet!</SubClaim>
        </Text>
      }
      control={
        <ButtonPink onClick={() => act(A._infoGeneral(A._next(unit)))}>
          Next
        </ButtonPink>
      }
    />
  );
};
