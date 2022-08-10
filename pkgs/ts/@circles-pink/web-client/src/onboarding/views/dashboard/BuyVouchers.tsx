import { VoucherProvidersResult } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { getData } from '@circles-pink/state-machine/output/RemoteReport';
import {
  VoucherOffer,
  VoucherProvider,
} from '@circles-pink/state-machine/output/VoucherServer.Types';
import React, { ReactElement } from 'react';
import { css, styled } from 'twin.macro';
import { Button } from '../../../components/forms';
import { Claim } from '../../../components/text';
import { Theme } from '../../../context/theme';

type BuyVouchersProps = {
  providers: VoucherProvidersResult;
  theme: Theme;
};

export const BuyVouchers = ({
  providers,
  theme,
}: BuyVouchersProps): ReactElement => {
  const providers_: VoucherProvider[] = getData([] as VoucherProvider[])(
    providers as any
  );

  return (
    <>
      <Claim color={theme.baseColor}>Buy Vouchers:</Claim>
      {providers_.map(provider => {
        return (
          <div key={provider.id}>
            <OfferContainer elementCount={provider.availableOffers.length}>
              {provider.availableOffers.map(offer => {
                return (
                  <Offer theme={theme} key={offer.amount}>
                    <Logo src={provider.logoUrl} />
                    <div>
                      {offer.amount}€ {provider.name} voucher
                    </div>
                    <div>{offer.countAvailable} vouchers left!</div>
                  </Offer>
                );
              })}
            </OfferContainer>
          </div>
        );
      })}
    </>
  );
};

type OfferContainerProps = {
  elementCount: number;
};

const OfferContainer = styled.div<OfferContainerProps>(({ elementCount }) => [
  css`
    display: grid;
    grid-template-columns: repeat(${elementCount}, 1fr);
  `,
]);

type OfferProps = {
  theme: Theme;
};

const Offer = styled.button<OfferProps>(({ theme }) => {
  const borderWithoutLeft = `
    border-top: 1px solid ${theme.darkColor};
    border-bottom: 1px solid ${theme.darkColor};
    border-right: 1px solid ${theme.darkColor};
  `;

  return [
    css`
      outline: none;
      border: none;
      cursor: pointer;
      font-size: 2rem;
      color: ${theme.textColorDark};
      background-color: ${theme.lightColor};
      ${borderWithoutLeft}
      padding: 1rem;
      &:first-of-type {
        border: 1px solid ${theme.darkColor};
      }
      &:last-of-type {
        ${borderWithoutLeft}
      }
      &:hover {
        background-color: ${theme.textColorLight};
        color: ${theme.baseColor};
      }
    `,
  ];
});

// -----------------------------------------------------------------------------
// UI Logo
// -----------------------------------------------------------------------------

const Logo = styled.img(() => [
  css`
    max-width: 10rem;
  `,
]);
