import React from 'react';
import { CenterText, JustifyAround } from '../../../components/helper';
import { Claim, Text } from '../../../components/text';
import { Theme } from '../../../context/theme';
import { DashboardProps } from '../dashboard';
import QrCode from 'react-qrcode-svg';
import tw, { styled } from 'twin.macro';
import { _Address } from '@circles-pink/state-machine/src';

// -----------------------------------------------------------------------------
// Receive Circles
// -----------------------------------------------------------------------------

type ReceiveProps = Omit<DashboardProps, 'buyVoucherEurLimit'> & {
  theme: Theme;
};

export const Receive = ({ state, theme }: ReceiveProps) => {
  const safeAddress = _Address.addrToString(
    _Address.Address(state.user.safeAddress)
  );
  return (
    <>
      <Claim color={theme.baseColor}>Receive Circles</Claim>
      <br />
      <CenterText>
        <Text>Show this QR code to the sender:</Text>
      </CenterText>
      <JustifyAround>
        <QrCode
          data={safeAddress}
          height="200"
          width="200"
          fgColor="gray"
          bgColor="white"
        />
      </JustifyAround>
      <CenterText>
        <Address>{safeAddress}</Address>
      </CenterText>
    </>
  );
};

const Address = styled.span(() => [
  tw`text-gray-900 mb-2 lg:text-base text-sm`,
]);
