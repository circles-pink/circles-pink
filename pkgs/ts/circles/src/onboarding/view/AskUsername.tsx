import React, { ReactElement } from 'react';
import { DialogCard } from '../DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { Button, Input } from '../../components/forms';

type AskUsernameProps = {
  username: string;
  debug?: string;
  setUsername: (n: string) => void;
  back: () => void;
  // next: () => void
};

export const AskUsername = ({
  username,
  debug,
  setUsername,
  back,
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
        <Input
          type="text"
          value={username}
          placeholder={'Your amazing username'}
          onChange={e => setUsername(e.target.value)}
        />
      }
      debug={<pre>{debug}</pre>}
      control={
        <>
          <Button onClick={() => back()}>Back</Button>
          {/* <Button onClick={() => next()}>Next</Button> */}
        </>
      }
    />
  );
};
