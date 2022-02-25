import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';

type AskUsernameProps = {
  username: string;
  indicatorColor: string;
  setUsername: (n: string) => void;
  back: () => void;
  next: () => void;
  debug?: string;
};

export const AskUsername = ({
  username,
  setUsername,
  indicatorColor,
  back,
  next,
  debug,
}: AskUsernameProps): ReactElement => {
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
          indicatorColor={indicatorColor}
          type="text"
          value={username}
          placeholder={'Your amazing username'}
          onChange={e => setUsername(e.target.value)}
        />
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
