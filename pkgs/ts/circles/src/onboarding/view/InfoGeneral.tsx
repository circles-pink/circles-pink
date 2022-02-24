import React, { ReactElement } from 'react';
import { ButtonPink } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';

type InfoGeneralProps = {
  next: () => void;
};

export const InfoGeneral = ({ next }: InfoGeneralProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>Welcome to Circles</Claim>
          <SubClaim>Let's get you a circles Wallet!</SubClaim>
        </Text>
      }
      control={<ButtonPink onClick={() => next()}>Next</ButtonPink>}
    />
  );
};
