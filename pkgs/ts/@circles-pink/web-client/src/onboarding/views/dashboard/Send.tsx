import { mdiCashFast, mdiQrcodeScan } from '@mdi/js';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import React, { ReactElement, useEffect, useState } from 'react';
import { Button, Input } from '../../../components/forms';
import { JustifyEnd } from '../../../components/helper';
import { Claim, SubClaim } from '../../../components/text';
import { Theme } from '../../../context/theme';
import { mapResult } from '../../utils/mapResult';
import { convertTcToCrc } from '../../utils/timeCircles';
const Web3 = require('web3');
import { t } from 'i18next';
import Icon from '@mdi/react';
import styled from '@emotion/styled';
import tw from 'twin.macro';
import {
  Address,
  DashboardState,
  CirclesAction,
  DashboardAction,
  _Address,
  Maybe,
  _Nullable,
  _StateMachine,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';

// -----------------------------------------------------------------------------
// Send Circles
// -----------------------------------------------------------------------------

const { _circlesAction, _dashboardAction } = _StateMachine;

let QrScanner: React.ReactElement<
  unknown,
  string | React.JSXElementConstructor<unknown>
>;

export type SendProps = {
  theme: Theme;
  overwriteTo?: Address;
  state: DashboardState;
  act: (ac: DashboardAction) => void;
};

export const Send = ({ state, act, theme, overwriteTo }: SendProps) => {
  // State
  const [from, _] = useState<Address>(_Address.Address(state.user.safeAddress));
  const [to, setTo] = useState<string>(
    overwriteTo ? _Address.addrToString(overwriteTo) : ''
  );
  const [value, setValue] = useState<number>(0);
  const [paymentNote, setPaymentNote] = useState<string>('');
  const [scannerOpen, setScannerOpen] = useState<boolean>(false);

  const optionAddr: Address | null = pipe(
    to,
    _Address.parseAddress,
    _Nullable.toNullable
  );

  // QR Scanner

  useEffect(() => {
    // Needs to be required after mount
    QrScanner = require('eth-qr-scanner');
  }, []);

  // Util
  const transact = (fromAddr: Address, toAddr: Address) =>
    act(
      _dashboardAction._transfer({
        from: fromAddr,
        to: toAddr,
        value: new Web3.utils.BN(
          Web3.utils.toWei(convertTcToCrc(value).toString(), 'ether')
        ),
        paymentNote,
      })
    );

  const handleScan = (data: string) => {
    if (data) {
      setTo(data);
      setScannerOpen(false);
    }
  };
  const handleError = (err: unknown) => {
    console.error(err);
  };

  // Render
  return (
    <>
      <Claim color={theme.baseColor}>{t('dashboard.sendClaim')}</Claim>
      <div>
        {scannerOpen && (
          //@ts-ignore
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
            e.key === 'Enter' && optionAddr && transact(from, optionAddr)
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
          e.key === 'Enter' && optionAddr && transact(from, optionAddr)
        }
      />
      <Input
        type="string"
        value={paymentNote}
        placeholder={'Payment Note'}
        onChange={e => setPaymentNote(e.target.value)}
        onKeyPress={e =>
          e.key === 'Enter' && optionAddr && transact(from, optionAddr)
        }
      />
      <JustifyEnd>
        <Button
          prio={'high'}
          theme={theme}
          state={mapResult(state.transferResult)}
          icon={mdiCashFast}
          onClick={() => optionAddr && transact(from, optionAddr)}
        >
          {t('dashboard.sendButton')}
        </Button>
      </JustifyEnd>
    </>
  );
};

const InputRow = styled.div(() => [tw`flex`]);
const IconContainer = styled.div(() => [tw`ml-2 cursor-pointer`]);
