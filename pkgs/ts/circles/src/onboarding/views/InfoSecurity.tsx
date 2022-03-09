import { UserData } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement } from 'react';
import { ButtonGray, ButtonPink } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { unit } from 'generated/output/Data.Unit';
import { Orientation, FadeIn } from '../../components/animation/FadeIn';
import { getIncrementor } from '../utils/getCounter';
import { directionToOrientation } from '../utils/directionToOrientation';
import { t } from 'i18next';

type InfoSecurityProps = {
  state: UserData;
  act: (ac: A.CirclesAction) => void;
};

export const InfoSecurity = ({
  state,
  act,
}: InfoSecurityProps): ReactElement => {
  const orientation: Orientation = directionToOrientation(state.direction);
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim>{t('infoSecurity.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('infoSecurity.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            <ButtonGray onClick={() => act(A._infoSecurity(A._prev(unit)))}>
              {t('prevButton')}
            </ButtonGray>

            <ButtonPink onClick={() => act(A._infoSecurity(A._next(unit)))}>
              {t('nextButton')}
            </ButtonPink>
          </>
        </FadeIn>
      }
    />
  );
};
