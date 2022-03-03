import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { unit } from 'generated/output/Data.Unit';
import { getWords, keyToMnemonic } from 'generated/output/Wallet.PrivateKey';
import React, { ReactElement } from 'react';
import { Direction, FadeIn } from '../../components/animation/FadeIn';
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
  const direction: Direction = state.animation as Direction;
  const incrementBy = 0.05;
  const getDelay = (
    n => () =>
      (n += incrementBy)
  )(0 - incrementBy);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn direction={direction} delay={getDelay()}>
            <Claim>One key to rule them all...</Claim>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <SubClaim>
              Anyone with these words has full control over your Trustnetwork
              and can spend your $CRC!
            </SubClaim>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <SubClaim>
              You should write this on a sheet of paper to keep your Coins save.
            </SubClaim>
          </FadeIn>

          <FadeIn direction={direction} delay={getDelay()}>
            <pre>{getWords(keyToMnemonic(state.privateKey)).join(' ')}</pre>
          </FadeIn>
        </Text>
      }
      control={
        <>
          <FadeIn direction={direction} delay={getDelay()}>
            <ButtonGray onClick={() => act(A._infoSecurity(A._prev(unit)))}>
              Back
            </ButtonGray>
          </FadeIn>

          {/* <FadeIn direction={direction} delay={getDelay()}>
            <ButtonPink onClick={() => next()}>Next</ButtonPink>
          </FadeIn> */}
        </>
      }
    />
  );
};

// back={() => act(A._infoSecurity(A._prev(unit)))}
