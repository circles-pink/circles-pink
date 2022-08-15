import { VoucherProvidersResult } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { getData } from '@circles-pink/state-machine/output/RemoteReport';
import {
  VoucherOffer,
  VoucherProvider,
} from '@circles-pink/state-machine/output/VoucherServer.Types';
import React, { ReactElement, useEffect, useState } from 'react';
import { css, styled } from 'twin.macro';
import { Overlay, SelectedOffer } from '.';
import { Button } from '../../../components/forms';
import { Claim, SubClaim } from '../../../components/text';
import { Theme } from '../../../context/theme';

type BuyVouchersProps = {
  providers: VoucherProvidersResult;
  theme: Theme;
  initializeVoucherOrder: (selectedOffer: SelectedOffer) => void;
  availableBalance: number;
};

export const BuyVouchers = ({
  providers,
  theme,
  initializeVoucherOrder,
  availableBalance,
}: BuyVouchersProps): ReactElement => {
  const [providers_, setProviders_] = useState<VoucherProvider[]>(
    getData([] as VoucherProvider[])(providers as any)
  );

  useEffect(() => {
    const providerResult = getData([] as VoucherProvider[])(providers as any);
    if (providerResult.length > 0) {
      setProviders_(providerResult);
    }
  }, [providers]);

  if (providers_.length === 0) {
    return <SubClaim>Currently there were no vouchers available.</SubClaim>;
  }

  return (
    <>
      <Claim color={theme.baseColor}>Buy Vouchers:</Claim>
      {providers_.map(provider => {
        const sortedOffers = [...provider.availableOffers].sort(
          (a, b) => a.amount - b.amount
        );

        return (
          <div key={provider.id}>
            <OfferContainer elementCount={provider.availableOffers.length}>
              {sortedOffers.map(offer => {
                const userCanBuy = availableBalance - offer.amount * 10 > 0;

                return (
                  <Offer
                    enabled={userCanBuy}
                    theme={theme}
                    key={`${provider.id}-${offer.amount}`}
                    onClick={() =>
                      userCanBuy && initializeVoucherOrder([provider, offer])
                    }
                  >
                    <Logo src={provider.logoUrl} />
                    <VoucherText fontSize={1.25}>
                      {offer.amount}€ {provider.name} voucher
                    </VoucherText>
                    <VoucherText fontSize={1}>
                      {offer.countAvailable} vouchers left!
                    </VoucherText>
                    <br />

                    <VoucherText fontSize={1.5}>
                      {userCanBuy
                        ? `Buy for ${offer.amount * 10} Circles`
                        : `You need ${(
                            offer.amount * 10 -
                            availableBalance
                          ).toFixed(2)} Circles`}
                    </VoucherText>
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
  enabled: boolean;
};

const Offer = styled.button<OfferProps>(({ theme, enabled }) => {
  const borderTopBottomRight = `
    border-top: 1px solid ${theme.darkColor};
    border-bottom: 1px solid ${theme.darkColor};
    border-right: 1px solid ${theme.darkColor};
  `;

  return [
    css`
      cursor: ${enabled ? 'pointer' : 'not-allowed'};
      font-size: 2rem;
      color: ${theme.textColorDark};
      background-color: ${theme.lightColor};
      padding: 1rem;
      ${enabled &&
      `&:hover {
        background-color: ${theme.textColorLight};
        color: ${theme.baseColor};
      }`}

      outline: none;
      border: none;
      ${borderTopBottomRight}
      &:first-of-type {
        border: 1px solid ${theme.darkColor};
      }
      &:last-of-type {
        ${borderTopBottomRight}
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

// -----------------------------------------------------------------------------
// UI Text
// -----------------------------------------------------------------------------

const VoucherText = styled.p<{ fontSize: number }>(({ fontSize }) => [
  css`
    font-size: ${fontSize}rem;
    margin: 0;
    padding: 0;
  `,
]);