import { CirclesAction } from 'generated/output/CirclesPink.StateMachine.Action';
import { UserData } from 'generated/output/CirclesPink.StateMachine.State';
import { getWords, keyToMnemonic } from 'generated/output/Wallet.PrivateKey';
import React, { ReactElement } from 'react';
import {
  ButtonGray,
  // ButtonPink
} from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';

type MagicWordsProps = {
  //back: () => void;
  // next: () => void;
  state: UserData;
  act: (ac: CirclesAction) => void;
};

export const MagicWords = ({
  state,
  act,
}: // next,
//back,
MagicWordsProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>Let's talk about Security!!!!!!</Claim>
          <SubClaim>Most important: Keep your seedphrase save!</SubClaim>
          <pre>{getWords(keyToMnemonic(state.privateKey)).join(' ')}</pre>
        </Text>
      }
      control={
        <>
          {/* <ButtonGray onClick={() => back()}>Back</ButtonGray> */}
          {/* <ButtonPink onClick={() => next()}>Next</ButtonPink> */}
        </>
      }
    />
  );
};

//back={() => act(A._infoSecurity(A._prev(unit)))}
