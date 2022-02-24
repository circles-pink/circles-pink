import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, Input } from '../../components/forms';

type AskEmailProps = {
  email: string;
  debug?: string;
  setEmail: (n: string) => void;
  back: () => void;
  // next: () => void;
};

export const AskEmail = ({
  email,
  debug,
  setEmail,
  back,
}: // next,
AskEmailProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>How can I contact you?</Claim>
          <SubClaim>Please submit a valid e-mail address.</SubClaim>
        </Text>
      }
      interaction={
        <Input
          type="text"
          value={email}
          placeholder={'Enter your E-Mail'}
          onChange={e => setEmail(e.target.value)}
        />
      }
      debug={<pre>{debug}</pre>}
      control={
        <>
          <ButtonGray onClick={() => back()}>Back</ButtonGray>
          {/* <ButtonPink onClick={() => next()}>Next</ButtonPink> */}
        </>
      }
    />
  );
};
