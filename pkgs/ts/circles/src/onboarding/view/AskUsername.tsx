import { UserData } from 'generated/output/CirclesPink.StateMachine.State';
import * as A from 'generated/output/CirclesPink.StateMachine.Action';
import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';
import { mapIndicatorColors } from '../mapIndicatorColors';
import { unit } from 'generated/output/Data.Unit';

type AskUsernameProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskUsername = ({ state, act }: AskUsernameProps): ReactElement => {
  return (
    <DialogCard
      text={
        <Text>
          <Claim>Tell me your Name!</Claim>
          <SubClaim>Choose wisely, you can not change it later!</SubClaim>
        </Text>
      }
      interaction={
        <InputWithProps
          indicatorColor={mapIndicatorColors(state.usernameApiResult)}
          type="text"
          value={state.username}
          placeholder={'Your amazing username'}
          onChange={e => act(A._askUsername(A._setUsername(e.target.value)))}
        />
      }
      debug={<pre>{JSON.stringify(state.usernameApiResult, null, 2)}</pre>}
      control={
        <>
          <ButtonGray onClick={() => act(A._askUsername(A._prev(unit)))}>
            Back
          </ButtonGray>
          <ButtonPink onClick={() => act(A._askUsername(A._next(unit)))}>
            Next
          </ButtonPink>
        </>
      }
    />
  );
};
