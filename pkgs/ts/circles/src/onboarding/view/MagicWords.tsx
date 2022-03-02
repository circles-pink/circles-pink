import * as A from 'generated/output/CirclesPink.StateMachine.Action';
import { UserData } from 'generated/output/CirclesPink.StateMachine.State';
import { unit } from 'generated/output/Data.Unit';
import { getWords, keyToMnemonic } from 'generated/output/Wallet.PrivateKey';
import React, { ReactElement } from 'react';
import {
  ButtonGray,
  // ButtonPink
} from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';

type MagicWordsProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const MagicWords = ({ state, act }: MagicWordsProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>One key to rule them all...</Claim>
          <SubClaim>
            Anyone with these words has full control over your Trustnetwork and
            can spend your $CRC!
          </SubClaim>
          <SubClaim>
            You should write this on a sheet of paper to keep your Coins save.
          </SubClaim>
          <pre>{getWords(keyToMnemonic(state.privateKey)).join(' ')}</pre>
        </Text>
      }
      control={
        <>
          <ButtonGray onClick={() => act(A._infoSecurity(A._prev(unit)))}>
            Back
          </ButtonGray>
          {/* <ButtonPink onClick={() => next()}>Next</ButtonPink> */}
        </>
      }
    />
  );
};

// back={() => act(A._infoSecurity(A._prev(unit)))}
