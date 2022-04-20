import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useContext } from 'react';
import { Button } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { DialogCard } from '../../components/DialogCard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { DashboardState } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';

type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
};

export const Dashboard = ({ state, act }: DashboardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  return (
    <DialogCard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            <Claim color={theme.baseColor}>{t('dashboard.claim')}</Claim>
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('dashboard.subClaim')}</SubClaim>
          </FadeIn>
        </Text>
      }
      control={
        <FadeIn orientation={orientation} delay={getDelay()}>
          <>
            <Button
              color={theme.baseColor}
              onClick={() => act(A._dashboard(A._getTrusts(unit)))}
            >
              {t('getTrustsButton')}
            </Button>
          </>
        </FadeIn>
      }
    />
  );
};