import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { unit } from 'generated/output/Data.Unit';
import { getWords, keyToMnemonic } from 'generated/output/Wallet.PrivateKey';
import React, { ReactElement } from 'react';
import tw from 'twin.macro';
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

const WordGrit = tw.div`flex flex-wrap items-center`;
const Word = tw.div`lg:w-3/12 w-3/6 p-2`;

export const MagicWords = ({ state, act }: MagicWordsProps): ReactElement => {
  const direction: Direction =
    state.direction.type === 'forwards' ? 'left' : 'right';

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

          <WordGrit>
            {getWords(keyToMnemonic(state.privateKey)).map((word, index) => {
              return (
                <Word key={`${word}-${index}`}>
                  <FadeIn direction={direction} delay={getDelay()}>
                    <span>{word}</span>
                  </FadeIn>
                </Word>
              );
            })}
          </WordGrit>
        </Text>
      }
      control={
        <FadeIn direction={direction} delay={getDelay()}>
          <>
            <ButtonGray onClick={() => act(A._infoSecurity(A._prev(unit)))}>
              Back
            </ButtonGray>

            {/* <ButtonPink onClick={() => act(A._infoSecurity(A._next(unit)))}>
              Next
            </ButtonPink> */}
          </>
        </FadeIn>
      }
    />
  );
};
