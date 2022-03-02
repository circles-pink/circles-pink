import { UserData } from 'generated/output/CirclesPink.StateMachine.State';
import * as A from 'generated/output/CirclesPink.StateMachine.Action';
import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';
import { LabeledCheckbox } from '../../components/Checkbox';
import { mapIndicatorColors } from '../mapIndicatorColors';
import { unit } from 'generated/output/Data.Unit';

type AskEmailProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskEmail = ({ state, act }: AskEmailProps): ReactElement => {
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
            indicatorColor={mapIndicatorColors(state.emailApiResult)}
            type="text"
            value={state.email}
            placeholder={'Enter your E-Mail'}
            onChange={e => act(A._askEmail(A._setEmail(e.target.value)))}
          />
          <LabeledCheckbox
            label="Accept terms"
            checked={state.terms}
            setChecked={() => act(A._askEmail(A._setTerms(unit)))}
          />
          <LabeledCheckbox
            label="Accept privacy"
            checked={state.privacy}
            setChecked={() => act(A._askEmail(A._setPrivacy(unit)))}
          />
        </>
      }
      debug={<pre>{JSON.stringify(state.emailApiResult, null, 2)}</pre>}
      control={
        <>
          <ButtonGray onClick={() => act(A._askEmail(A._prev(unit)))}>
            Back
          </ButtonGray>
          <ButtonPink onClick={() => act(A._askEmail(A._next(unit)))}>
            Next
          </ButtonPink>
        </>
      }
    />
  );
};
