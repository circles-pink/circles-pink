import { DefaultView } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import {
  Voucher,
  VoucherProvider,
} from '@circles-pink/state-machine/output/VoucherServer.Types';
import { FadeIn, getIncrementor } from 'anima-react';
import React, { ReactElement, useEffect, useState } from 'react';
import { css, styled } from 'twin.macro';
import { Claim } from '../../../components/text';
import { Theme } from '../../../context/theme';

const EXAMPLE_LOGO =
  'https://cdn.shopify.com/s/files/1/0260/0819/1060/files/LOGO_GOOD_BUY_Farbe_rgb_Unterzeile_540x.png?v=1654701435';

type ListVouchersProps = {
  vouchersResult: DefaultView['vouchersResult'];
  providers: Array<VoucherProvider>;
  theme: Theme;
};

export const ListVouchers = ({
  vouchersResult,
  theme,
}: ListVouchersProps): ReactElement | null => {
  const [vouchers, setVouchers] = useState<Array<Voucher>>(
    mapResult(vouchersResult)
  );

  useEffect(() => {
    if (vouchersResult.type === 'success') {
      setVouchers(mapResult(vouchersResult));
    }
  }, [vouchersResult]);

  // animation
  const getDelay = getIncrementor(0, 0.25);

  return (
    <>
      <Claim color={theme.baseColor}>Your Vouchers:</Claim>
      <VoucherContainer>
        {vouchers.map(voucher => (
          <FadeIn orientation={'left'} delay={getDelay()} key={voucher.code}>
            <VoucherCard
              theme={theme}
              left={<Logo src={EXAMPLE_LOGO} />}
              center={<VoucherContent theme={theme} voucher={voucher} />}
              right={<VoucherAmount amount={voucher.amount} theme={theme} />}
            />
          </FadeIn>
        ))}
      </VoucherContainer>
    </>
  );
};

const mapResult = (
  vouchersResult: DefaultView['vouchersResult']
): Array<Voucher> => {
  switch (vouchersResult.type) {
    case 'success':
      return vouchersResult.value.data;
    case 'loading':
    case 'failure':
    case 'notAsked':
    default:
      return [];
  }
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const VoucherContainer = styled.div(() => [
  css`
    display: flex;
    flex-wrap: wrap;
    gap: 1rem;
  `,
]);

// -----------------------------------------------------------------------------
// UI VoucherCard
// -----------------------------------------------------------------------------

type VoucherCardProps = {
  theme: Theme;
  left: ReactElement;
  center: ReactElement;
  right: ReactElement;
};

const VoucherCard = ({ theme, left, center, right }: VoucherCardProps) => {
  // animation
  const getDelay = getIncrementor(0.25, 0.1);

  return (
    <Card>
      <BorderDot theme={theme} position={'left'} />
      <CardContent>
        <FadeIn orientation={'left'} delay={getDelay()}>
          {left}
        </FadeIn>
        <FadeIn orientation={'left'} delay={getDelay()}>
          {center}
        </FadeIn>
        <FadeIn orientation={'left'} delay={getDelay()}>
          {right}
        </FadeIn>
      </CardContent>
      <BorderDot theme={theme} position={'right'} />
    </Card>
  );
};

const Card = styled.div(() => [
  css`
    background-color: white;
    padding: 1rem 2rem;
    min-width: 20rem;
    position: relative;
  `,
]);

const CardContent = styled.div(() => [
  css`
    min-height: 5rem;
    display: grid;
    gap: 1rem;
    grid-template-columns: 1.5fr 2fr 0.25fr;
    align-items: center;
  `,
]);

type BorderDotProps = {
  theme: Theme;
  position: 'right' | 'left';
};

const BorderDot = styled.div<BorderDotProps>(({ theme, position }) => [
  css`
    background-color: ${theme.lightColor};
    height: 2rem;
    width: 2rem;
    border-radius: 50%;
    position: absolute;
    top: calc(50% - 1rem);
    ${position}: -1rem;
  `,
]);

// -----------------------------------------------------------------------------
// UI Logo
// -----------------------------------------------------------------------------

const Logo = styled.img(() => [
  css`
    max-width: 5rem;
  `,
]);

// -----------------------------------------------------------------------------
// UI Content
// -----------------------------------------------------------------------------

type VoucherContentProps = {
  voucher: Voucher;
  theme: Theme;
};

const VoucherContent = ({
  voucher,
  theme,
}: VoucherContentProps): ReactElement => {
  return (
    <VoucherContentContainer theme={theme}>
      <VoucherText theme={theme}>{voucher.providerId}</VoucherText>
      <VoucherText theme={theme}>{voucher.code}</VoucherText>
    </VoucherContentContainer>
  );
};

const VoucherText = styled.p<{ theme: Theme }>(({ theme }) => [
  css`
    color: ${theme.textColorDark};
    font-size: 1.25rem;
    margin: 0;
    padding: 0;
  `,
]);

const VoucherContentContainer = styled.div<{ theme: Theme }>(({ theme }) => [
  css`
    border-left: 6px dotted ${theme.baseColor};
    padding-left: 1rem;
  `,
]);

// -----------------------------------------------------------------------------
// UI Content
// -----------------------------------------------------------------------------

type VoucherAmountProps = {
  amount: number;
  theme: Theme;
};

const VoucherAmount = (props: VoucherAmountProps): ReactElement => {
  return <Amount {...props}>{props.amount}€</Amount>;
};

const Amount = styled.div<VoucherAmountProps>(({ theme }) => [
  css`
    text-align: center;
    transform: rotate(-90deg);
    color: ${theme.baseColor};
    font-size: 2rem;
    font-weight: 600;
  `,
]);
