import * as A from 'generated/output/CirclesPink.Garden.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';
import React, { ReactElement, useContext, useEffect } from 'react';
import { Button } from '../../components/forms';
import { Claim, SubClaim, Text } from '../../components/text';
import { UserDashboard } from '../../components/UserDashboard';
import { FadeIn } from 'anima-react';
import { Orientation } from 'anima-react/dist/components/FadeIn';
import { DashboardState } from 'generated/output/CirclesPink.Garden.StateMachine.State';
import { getIncrementor } from '../utils/getCounter';
import { t } from 'i18next';
import { ThemeContext } from '../../context/theme';
import { Graph, TrustGraph } from '../../components/TrustGraph';
import { unsafeAddrFromString } from 'generated/output/Wallet.PrivateKey';

type DashboardProps = {
  state: DashboardState;
  act: (ac: A.CirclesAction) => void;
};

export const Dashboard = ({ state, act }: DashboardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const orientation: Orientation = 'left';
  const getDelay = getIncrementor(0, 0.05);

  useEffect(() => {
    act(A._dashboard(A._getTrusts(unit))); // Should be done in control
    setInterval(() => act(A._dashboard(A._getTrusts(unit))), 15000);
  }, []);

  // const graph: Graph = new Map([[state.user.safeAddress, state.trusts]]);

  return (
    <UserDashboard
      text={
        <Text>
          <FadeIn orientation={orientation} delay={getDelay()}>
            {state.user.username ? (
              <Claim color={theme.baseColor}>
                {t('dashboard.greet')}
                {` ${state.user.username}!`}
              </Claim>
            ) : (
              <Claim color={theme.baseColor}>{t('dashboard.claim')}</Claim>
            )}
          </FadeIn>

          <FadeIn orientation={orientation} delay={getDelay()}>
            <SubClaim>{t('dashboard.subClaim')}</SubClaim>
          </FadeIn>

          {/* <FadeIn orientation={orientation} delay={getDelay()}>
            <TrustGraph graph={graph} />
          </FadeIn> */}
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
      // debug={<pre>{JSON.stringify(state, null, 2)}</pre>}
    />
  );
};
