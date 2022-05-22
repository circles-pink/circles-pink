import React from 'react';
import { CenterText, JustifyAround } from '../../../components/helper';
import { Claim, Text } from '../../../components/text';
import { Theme } from '../../../context/theme';
import { DashboardProps } from '../Dashboard';
import QrCode from 'react-qrcode-svg';
import tw, { css, styled } from 'twin.macro';

// -----------------------------------------------------------------------------
// Receive Circles
// -----------------------------------------------------------------------------

type ReceiveProps = DashboardProps & { theme: Theme };

export const Receive = ({ state, theme }: ReceiveProps) => {
  return (
    <>
      <Claim color={theme.baseColor}>Receive Circles</Claim>
      <br />
      <CenterText>
        <Text>Show this QR code to the sender:</Text>
      </CenterText>
      <JustifyAround>
        <QrCode
          data={state.user.safeAddress}
          height="200"
          width="200"
          fgColor="gray"
          bgColor="white"
        />
      </JustifyAround>
      <CenterText>
        <Address>{state.user.safeAddress}</Address>
      </CenterText>
    </>
  );
};

const Address = styled.span(() => [
  tw`text-gray-900 mb-2 lg:text-base text-sm`,
]);
