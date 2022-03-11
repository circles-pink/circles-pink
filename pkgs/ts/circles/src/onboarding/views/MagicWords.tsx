import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { unit } from 'generated/output/Data.Unit';
import { getWords, keyToMnemonic } from 'generated/output/Wallet.PrivateKey';
import React, { ReactElement, useState } from 'react';
import tw from 'twin.macro';
import { Orientation, FadeIn } from '../../components/animation/FadeIn';
import {
  ButtonGray,
  ButtonPinkFullWidth,
  // ButtonPink
} from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { directionToOrientation } from '../utils/directionToOrientation';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';

type MagicWordsProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

const WordGrid = tw.div`grid xl:grid-cols-6 lg:grid-cols-4 md:grid-cols-3 sm:grid-cols-2 bg-green-100 py-2 pl-2 rounded-lg mb-4`;
const WordContainer = tw.div`py-1 mr-2`;
const Word = tw.div`p-2 bg-white w-full text-center rounded-full relative`;
const WordNumber = tw.span`select-none text-sm absolute left-2 text-gray-400`;
const ButtonRow = tw.div`flex`;

export const MagicWords = ({ state, act }: MagicWordsProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);
  const getWordDelay = getIncrementor(0, 0.02);

  const words = getWords(keyToMnemonic(state.privateKey));

  const [copyNotify, setCopyNotify] = useState('');

  const copyToClipboard = () => {
    navigator.clipboard.writeText(words.join(' '));
    setCopyNotify(t('magicWords.copiedInfo'));
    setTimeout(() => {
      setCopyNotify('');
    }, 1000);
  };

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim>{t('magicWords.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('magicWords.subClaim1')}</SubClaim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('magicWords.subClaim2')}</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            <ButtonGray onClick={() => act(A._infoSecurity(A._prev(unit)))}>
              {t('prevButton')}
            </ButtonGray>

            {/* <ButtonPink onClick={() => act(A._infoSecurity(A._next(unit)))}>
              {t('nextButton')}
            </ButtonPink> */}
          </>
        </FadeIn>
      }
      mainContent={
        <>
          <WordGrid>
            {words.map((word, index) => {
              return (
                <WordContainer key={`${word}-${index}`}>
                  <FadeIn orientation={'up'} delay={getWordDelay()}>
                    <Word>
                      <WordNumber>{index + 1}</WordNumber>
                      <span>{word}</span>
                    </Word>
                  </FadeIn>
                </WordContainer>
              );
            })}
          </WordGrid>
          <FadeIn orientation={'down'} delay={getWordDelay()}>
            <ButtonRow>
              <ButtonPinkFullWidth onClick={() => copyToClipboard()}>
                {copyNotify ? copyNotify : t('magicWords.copyBtn')}
              </ButtonPinkFullWidth>
              <ButtonPinkFullWidth
                onClick={() => act(A._magicWords(A._newPrivKey(unit)))}
              >
                {t('magicWords.newPhraseBtn')}
              </ButtonPinkFullWidth>
            </ButtonRow>
          </FadeIn>
        </>
      }
    />
  );
};
