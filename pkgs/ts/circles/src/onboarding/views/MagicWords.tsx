import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { unit } from 'generated/output/Data.Unit';
import { getWords, keyToMnemonic } from 'generated/output/Wallet.PrivateKey';
import React, { ReactElement } from 'react';
import tw from 'twin.macro';
import { Orientation, FadeIn } from '../../components/animation/FadeIn';
import {
  ButtonGray,
  // ButtonPink
} from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { directionToOrientation } from '../utils/directionToOrientation';
import { getIncrementor } from '../utils/getCounter';

type MagicWordsProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

const WordGrit = tw.div`flex flex-wrap items-center border-solid border-pink-500 border`;
const Word = tw.div`lg:w-3/12 w-3/6 p-2 border-solid border-pink-500 border p-1`;

export const MagicWords = ({ state, act }: MagicWordsProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);
  const getWordDelay = getIncrementor(0, 0.02);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim>One key to rule them all...</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>
              Anyone with these words has full control over your Trustnetwork
              and can spend your $CRC!
            </SubClaim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>
              You should write this on a sheet of paper to keep your Coins save.
            </SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
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
      mainContent={
        <WordGrit>
          {getWords(keyToMnemonic(state.privateKey)).map((word, index) => {
            return (
              <Word key={`${word}-${index}`}>
                <FadeIn orientation={'up'} delay={getWordDelay()}>
                  <span>{word}</span>
                </FadeIn>
              </Word>
            );
          })}
        </WordGrit>
      }
    />
  );
};
