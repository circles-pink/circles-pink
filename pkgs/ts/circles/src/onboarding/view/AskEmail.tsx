import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';
import { LabeledCheckbox } from '../../components/Checkbox';

type AskEmailProps = {
  email: string;
  terms: boolean;
  privacy: boolean;
  indicatorColor: string;
  setEmail: (n: string) => void;
  setTerms: () => void;
  setPrivacy: () => void;
  back: () => void;
  next: () => void;
  debug?: string;
};

export const AskEmail = ({
  email,
  terms,
  privacy,
  indicatorColor,
  setEmail,
  setTerms,
  setPrivacy,
  back,
  next,
  debug,
}: AskEmailProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>How do you want to be notified?</Claim>
          <SubClaim>Please submit a valid e-mail address.</SubClaim>
        </Text>
      }
      interaction={
        <>
          <InputWithProps
            indicatorColor={indicatorColor}
            type="text"
            value={email}
            placeholder={'Enter your E-Mail'}
            onChange={e => setEmail(e.target.value)}
          />
          <LabeledCheckbox
            label="Accept terms"
            checked={terms}
            setChecked={setTerms}
          />
          <LabeledCheckbox
            label="Accept privacy"
            checked={privacy}
            setChecked={setPrivacy}
          />
        </>
      }
      debug={<pre>{debug}</pre>}
      control={
        <>
          <ButtonGray onClick={() => back()}>Back</ButtonGray>
          <ButtonPink onClick={() => next()}>Next</ButtonPink>
        </>
      }
    />
  );
};
