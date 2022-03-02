import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement } from 'react';
import { ButtonGray, ButtonPink } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';
import { unit } from 'generated/output/Data.Unit';

type InfoSecurityProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const InfoSecurity = ({
  state,
  act,
}: InfoSecurityProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>Let's talk about Security</Claim>
          <SubClaim>Most important: Keep your seedphrase save!</SubClaim>
        </Text>
      }
      control={
        <>
          <ButtonGray onClick={() => act(A._infoSecurity(A._prev(unit)))}>
            Back
          </ButtonGray>
          <ButtonPink onClick={() => act(A._infoSecurity(A._next(unit)))}>
            Next
          </ButtonPink>
        </>
      }
    />
  );
};
