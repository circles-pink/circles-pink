import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import {
  CirclesState,
  UserData,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import { unit } from '@circles-pink/state-machine/output/Data.Unit';
import {
  getWords,
  keyToMnemonic,
} from '@circles-pink/state-machine/output/CirclesPink.Data.Mnemonic';
import React, { ReactElement, useContext, useState } from 'react';
import tw, { css, styled } from 'twin.macro';
import { FadeIn, ZoomIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { Button } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { directionToOrientation } from '../utils/directionToOrientation';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { Theme, ThemeContext } from '../../context/theme';
import { lighten } from '../utils/colorUtils';
import { OnboardingStepIndicator } from '../../components/layout';
import { TwoButtonRow } from '../../components/helper';
import { maybe } from '@circles-pink/state-machine/output/Data.Maybe';
import { StateMachineDebugger } from '../../components/StateMachineDebugger';
import { PrivateKey } from '@circles-pink/state-machine/output/CirclesPink.Data.PrivateKey.Type';

type MagicWordsProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
  skip: CirclesState['type'][];
};

export const MagicWords = ({
  state,
  act,
  skip,
}: MagicWordsProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);
  const getWordDelay = getIncrementor(0, 0.02);

  const words = (() =>
    maybe([] as string[])(pk => getWords(keyToMnemonic(pk as PrivateKey)))(
      state.privateKey
    ))();

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
      header={<OnboardingStepIndicator skipStates={skip} />}
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('magicWords.claim')}</Claim>
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
          <TwoButtonRow>
            <Button
              prio={'medium'}
              theme={theme}
              onClick={() => act(A._magicWords(A._prev(unit)))}
            >
              {t('prevButton')}
            </Button>

            <Button
              prio={'high'}
              theme={theme}
              onClick={() => act(A._magicWords(A._next(unit)))}
            >
              {t('magicWords.gotMyMagicWordsButton')}
            </Button>
          </TwoButtonRow>
        </FadeIn>
      }
      mainContent={
        <>
          <WordGrid theme={theme}>
            {words.map((word, index) => {
              return (
                <WordContainer key={`${word}-${index}`}>
                  <ZoomIn orientation={'down'} delay={getWordDelay()}>
                    <Word>
                      <WordNumber>{index + 1}</WordNumber>
                      <span>{word}</span>
                    </Word>
                  </ZoomIn>
                </WordContainer>
              );
            })}
          </WordGrid>
          <FadeIn orientation={'down'} delay={getWordDelay()}>
            <TwoButtonRow>
              <Button
                prio={'low'}
                theme={theme}
                fullWidth={true}
                onClick={() => act(A._magicWords(A._newPrivKey(unit)))}
              >
                {t('magicWords.newPhraseBtn')}
              </Button>
              <Button
                prio={'low'}
                theme={theme}
                fullWidth={true}
                onClick={() => copyToClipboard()}
              >
                {copyNotify ? (
                  <ZoomIn>
                    <span>{copyNotify}</span>
                  </ZoomIn>
                ) : (
                  t('magicWords.copyBtn')
                )}
              </Button>
            </TwoButtonRow>
          </FadeIn>
        </>
      }
      debug={<StateMachineDebugger state={state} />}
    />
  );
};

type WordGridProps = {
  theme: Theme;
};

const WordGrid = styled.div((props: WordGridProps) => {
  return [
    css`
      background: ${lighten(props.theme.lightColor)};
    `,
    tw`grid xl:grid-cols-6 lg:grid-cols-4 md:grid-cols-3 sm:grid-cols-2 py-2 pl-2 rounded-lg mb-4`,
  ];
});
const WordContainer = tw.div`py-1 mr-2`;
const Word = tw.div`p-2 bg-white w-full text-center text-black rounded-full relative`;
const WordNumber = tw.span`select-none text-sm absolute left-2 text-gray-400`;
