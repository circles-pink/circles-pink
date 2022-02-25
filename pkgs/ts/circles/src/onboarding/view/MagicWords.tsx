import React, { ReactElement } from 'react';
import {
  ButtonGray,
  // ButtonPink
} from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../DialogCard';

type MagicWordsProps = {
  back: () => void;
  // next: () => void;
};

export const MagicWords = ({
  // next,
  back,
}: MagicWordsProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>Let's talk about Security!!!!!!</Claim>
          <SubClaim>Most important: Keep your seedphrase save!</SubClaim>
        </Text>
      }
      control={
        <>
          <ButtonGray onClick={() => back()}>Back</ButtonGray>
          {/* <ButtonPink onClick={() => next()}>Next</ButtonPink> */}
        </>
      }
    />
  );
};
