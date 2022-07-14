import { mdiCashFast, mdiQrcodeScan } from '@mdi/js';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import React, { useState } from 'react';
import { Button, Input } from '../../../components/forms';
import { JustifyEnd } from '../../../components/helper';
import { Claim } from '../../../components/text';
import { Theme } from '../../../context/theme';
import { mapResult } from '../../utils/mapResult';
import { convertTcToCrc } from '../../utils/timeCircles';
const Web3 = require('web3');
import { DashboardProps } from '../Dashboard';
const QrScanner = require('eth-qr-scanner');
import { t } from 'i18next';
import {
  DefaultView,
  defaultView,
  addrToString,
  parseAddress,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import Icon from '@mdi/react';
import styled from '@emotion/styled';
import tw from 'twin.macro';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { Option } from 'fp-ts/lib/Option';
import { toFpTsOption } from '../../../utils/fpTs';

// -----------------------------------------------------------------------------
// Send Circles
// -----------------------------------------------------------------------------

export type SendProps = DashboardProps & {
  theme: Theme;
  overwriteTo?: Address;
};

export const Send = ({
  state: stateRaw,
  act,
  theme,
  overwriteTo,
}: SendProps) => {
  const state = (defaultView as any)(stateRaw) as DefaultView;

  // State
  const [from, _] = useState<Address>(stateRaw.user.safeAddress);
  const [to, setTo] = useState<string>(
    overwriteTo ? addrToString(overwriteTo) : ''
  );
  const [value, setValue] = useState<number>(0);
  const [paymentNote, setPaymentNote] = useState<string>('');
  const [scannerOpen, setScannerOpen] = useState<boolean>(false);

  const optionAddr: Option<Address> = toFpTsOption(parseAddress(to));

  // Util
  const transact = (fromAddr: Address, toAddr: Address) =>
    act(
      A._dashboard(
        A._transfer({
          from: fromAddr,
          to: toAddr,
          value: Web3.utils.toWei(convertTcToCrc(value).toString(), 'ether'),
          paymentNote,
        })
      )
    );

  const handleScan = (data: string) => {
    if (data) {
      setTo(data);
      setScannerOpen(false);
    }
  };
  const handleError = (err : unknown) => {
    console.error(err);
  };

  // Render
  return (
    <>
      <Claim color={theme.baseColor}>{t('dashboard.sendClaim')}</Claim>
      <div>
        {scannerOpen && (
          <QrScanner
            delay={300}
            onError={handleError}
            onScan={handleScan}
            style={{ width: '100%' }}
          />
        )}
      </div>
      <br />
      <InputRow>
        <Input
          type="text"
          value={to}
          placeholder={'To'}
          onChange={e => setTo(e.target.value)}
          onKeyPress={e =>
            e.key === 'Enter' &&
            optionAddr._tag === 'Some' &&
            transact(from, optionAddr.value)
          }
        />
        <IconContainer onClick={() => setScannerOpen(!scannerOpen)}>
          <Icon path={mdiQrcodeScan} size={1.5} color={theme.baseColor} />
        </IconContainer>
      </InputRow>
      <Input
        type="number"
        step=".01"
        value={value}
        placeholder={'Amount'}
        onChange={e => {
          const twoDecimals = parseFloat(e.target.value).toFixed(2);
          setValue(parseFloat(twoDecimals));
        }}
        onKeyPress={e =>
          e.key === 'Enter' &&
          optionAddr._tag === 'Some' &&
          transact(from, optionAddr.value)
        }
      />
      <Input
        type="string"
        value={paymentNote}
        placeholder={'Payment Note'}
        onChange={e => setPaymentNote(e.target.value)}
        onKeyPress={e =>
          e.key === 'Enter' &&
          optionAddr._tag === 'Some' &&
          transact(from, optionAddr.value)
        }
      />
      <JustifyEnd>
        <Button
          prio={'high'}
          theme={theme}
          state={mapResult(state.transferResult)}
          icon={mdiCashFast}
          onClick={() =>
            optionAddr._tag === 'Some' && transact(from, optionAddr.value)
          }
        >
          {t('dashboard.sendButton')}
        </Button>
      </JustifyEnd>
    </>
  );
};

const InputRow = styled.div(() => [tw`flex`]);
const IconContainer = styled.div(() => [tw`ml-2 cursor-pointer`]);
