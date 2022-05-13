import { mdiCashFast } from '@mdi/js';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import { addrToString } from '@circles-pink/state-machine/output/Wallet.PrivateKey';
import React, { useEffect, useState } from 'react';
import { Button, Input } from '../../../components/forms';
import { JustifyEnd } from '../../../components/helper';
import { Claim } from '../../../components/text';
import { Theme } from '../../../context/theme';
import { mapBalanceToBN } from '../../utils/balance';
import { mapResult } from '../../utils/mapResult';
import { convertTimeCirclesToCircles } from '../../utils/timeCircles';
import { DashboardProps } from '../Dashboard';
import { t } from 'i18next';
import {
  DefaultView,
  defaultView,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';

// -----------------------------------------------------------------------------
// Send Circles
// -----------------------------------------------------------------------------

export type SendProps = DashboardProps & {
  theme: Theme;
  overwriteTo?: string;
};

export const Send = ({
  state: stateRaw,
  act,
  theme,
  overwriteTo,
}: SendProps) => {
  const state = (defaultView as any)(stateRaw) as DefaultView;

  // State
  const [from, _] = useState<string>(addrToString(stateRaw.user.safeAddress));
  const [to, setTo] = useState<string>(overwriteTo || '');
  const [value, setValue] = useState<number>(0);
  const [paymentNote, setPaymentNote] = useState<string>('');

  // Util
  const transact = () =>
    act(
      A._dashboard(
        A._transfer({
          from,
          to,
          value: mapBalanceToBN(
            parseFloat(
              convertTimeCirclesToCircles(value, new Date().toString()).toFixed(
                2
              )
            )
          ),
          paymentNote,
        })
      )
    );

  // Render
  return (
    <>
      <Claim color={theme.baseColor}>{t('dashboard.sendClaim')}</Claim>
      <br />
      <Input
        type="text"
        value={to}
        placeholder={'To'}
        onChange={e => setTo(e.target.value)}
        onKeyPress={e => e.key === 'Enter' && transact()}
      />
      <Input
        type="number"
        step=".01"
        value={value}
        placeholder={'Amount'}
        onChange={e => {
          const twoDecimals = parseFloat(e.target.value).toFixed(2);
          setValue(parseFloat(twoDecimals));
        }}
        onKeyPress={e => e.key === 'Enter' && transact()}
      />
      <Input
        type="string"
        value={paymentNote}
        placeholder={'Payment Note'}
        onChange={e => setPaymentNote(e.target.value)}
        onKeyPress={e => e.key === 'Enter' && transact()}
      />
      <JustifyEnd>
        <Button
          prio={'high'}
          color={theme.baseColor}
          state={mapResult(state.transferResult)}
          icon={mdiCashFast}
          onClick={() => transact()}
        >
          {t('dashboard.sendButton')}
        </Button>
      </JustifyEnd>
    </>
  );
};
