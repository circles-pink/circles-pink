import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement } from 'react';
import { DialogCard } from '../../components/DialogCard';

import { Claim, SubClaim, Text } from '../../components/text';
import { ButtonGray, ButtonPink, InputWithProps } from '../../components/forms';
import { LabeledCheckbox } from '../../components/Checkbox';
import { mapIndicatorColors } from '../utils/mapIndicatorColors';
import { unit } from 'generated/output/Data.Unit';
import { Orientation, FadeIn } from '../../components/animation/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';
import { t } from 'i18next';

type AskEmailProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const AskEmail = ({ state, act }: AskEmailProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim>{t('askEmail.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('askEmail.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      interaction={
        <>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <InputWithProps
              autoFocus
              indicatorColor={mapIndicatorColors(state.emailApiResult)}
              type="text"
              value={state.email}
              placeholder={t('askEmail.emailPlaceholder')}
              onChange={e => act(A._askEmail(A._setEmail(e.target.value)))}
              onKeyPress={e =>
                e.key === 'Enter' && act(A._askEmail(A._next(unit)))
              }
            />
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <div>
              <LabeledCheckbox
                label={t('askEmail.termsLabel')}
                checked={state.terms}
                setChecked={() => act(A._askEmail(A._setTerms(unit)))}
              />
            </div>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <div>
              <LabeledCheckbox
                label={t('askEmail.privacyLabel')}
                checked={state.privacy}
                setChecked={() => act(A._askEmail(A._setPrivacy(unit)))}
              />
            </div>
          </FadeIn>
        </>
      }
      debug={<pre>{JSON.stringify(state.emailApiResult, null, 2)}</pre>}
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            <ButtonGray onClick={() => act(A._askEmail(A._prev(unit)))}>
              {t('prevButton')}
            </ButtonGray>

            <ButtonPink onClick={() => act(A._askEmail(A._next(unit)))}>
              {t('nextButton')}
            </ButtonPink>
          </>
        </FadeIn>
      }
    />
  );
};
