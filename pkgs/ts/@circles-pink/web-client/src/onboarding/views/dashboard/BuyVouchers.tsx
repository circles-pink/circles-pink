import { VoucherProvidersResult } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { getData } from '@circles-pink/state-machine/output/RemoteReport';
import { VoucherProvider } from '@circles-pink/state-machine/output/VoucherServer.Spec.Types';
import { t } from 'i18next';
import React, { ReactElement, useEffect, useState } from 'react';
import { css, styled } from 'twin.macro';
import { SelectedOffer } from '.';
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

  return (
    <>
      <Claim color={theme.baseColor}>
        {t('dashboard.voucherShop.buyDescription')}
      </Claim>
      {providers_.length > 0 ? (
        <>
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
                          userCanBuy &&
                          initializeVoucherOrder([provider, offer])
                        }
                      >
                        <Logo src={provider.logoUrl} />
                        <VoucherText fontSize={1.25}>
                          {offer.amount}€ {provider.name}{' '}
                          {t('dashboard.voucherShop.voucher')}
                        </VoucherText>
                        <VoucherText fontSize={1}>
                          {offer.countAvailable}{' '}
                          {t('dashboard.voucherShop.vouchersLeft')}
                        </VoucherText>
                        <br />

                        <VoucherText fontSize={1.5}>
                          {userCanBuy
                            ? `${t('dashboard.voucherShop.buyFor')} ${
                                offer.amount * 10
                              } Circles`
                            : `${t('dashboard.voucherShop.youNeed')} ${(
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
      ) : (
        <SubClaim>{t('dashboard.voucherShop.buyNoVouchers')}</SubClaim>
      )}
    </>
  );
};

type OfferContainerProps = {
  elementCount: number;
};

const OfferContainer = styled.div<OfferContainerProps>(({ elementCount }) => [
  css`
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(15rem, 1fr));
    grid-gap: 1px;
  `,
]);

type OfferProps = {
  theme: Theme;
  enabled: boolean;
};

const Offer = styled.button<OfferProps>(({ theme, enabled }) => {
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

      outline: 1px solid ${theme.textColorDark};
      border: none;
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
