import { CirclesState } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import { VoucherProvidersResult } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { DefaultView } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { getData } from '@circles-pink/state-machine/output/RemoteReport';
import {
  Voucher,
  VoucherProvider,
} from '@circles-pink/state-machine/output/VoucherServer.Types';
import { FadeIn, getIncrementor } from 'anima-react';
import { t } from 'i18next';
import React, {
  ReactElement,
  SetStateAction,
  useEffect,
  useState,
} from 'react';
import { css, styled } from 'twin.macro';
import { CirclesCurrency } from '../../../assets/CirclesCurrency';
import { CurrencySymbol } from '../../../components/CurrencySymbol';
import { LoadingCircles } from '../../../components/LoadingCircles';
import { Claim, LoadingText, SubClaim } from '../../../components/text';
import { Theme } from '../../../context/theme';

type ListVouchersProps = {
  vouchersResult: DefaultView['vouchersResult'];
  providers: VoucherProvidersResult;
  theme: Theme;
  justBoughtVoucher: boolean;
  setJustBoughtVoucher: React.Dispatch<SetStateAction<boolean>>;
};

export const ListVouchers = ({
  vouchersResult,
  theme,
  providers,
  justBoughtVoucher,
  setJustBoughtVoucher,
}: ListVouchersProps): ReactElement | null => {
  const [vouchers, setVouchers] = useState<Array<Voucher>>(
    mapResult(vouchersResult)
  );
  const [vouchersBeforePurchase, setVouchersBeforePurchase] = useState(
    vouchers.length
  );

  const providers_: VoucherProvider[] = getData([] as VoucherProvider[])(
    providers as any
  );

  useEffect(() => {
    if (justBoughtVoucher && vouchersBeforePurchase === vouchers.length) {
    } else if (
      justBoughtVoucher &&
      vouchersBeforePurchase !== vouchers.length
    ) {
      setJustBoughtVoucher(false);
    }
  }, [justBoughtVoucher, vouchers]);

  useEffect(() => {
    if (vouchersResult.type === 'success') {
      setVouchers(mapResult(vouchersResult));
    }
  }, [vouchersResult]);

  // animation
  const getDelay = getIncrementor(0, 0.25);

  return (
    <>
      <Claim color={theme.baseColor}>
        {t('dashboard.voucherShop.listDescription')}
      </Claim>
      {vouchers.length > 0 || justBoughtVoucher ? (
        <VoucherContainer>
          {vouchers.map((voucher, index) => {
            const provider = mapInfo(providers_, voucher.providerId);
            return (
              <FadeIn
                orientation={'left'}
                delay={getDelay()}
                key={`${voucher.code}-${index}`}
              >
                <VoucherCard
                  theme={theme}
                  left={<Logo src={provider?.logoUrl} />}
                  center={
                    <VoucherContent
                      provider={provider}
                      theme={theme}
                      voucher={voucher}
                    />
                  }
                  right={
                    <VoucherAmount amount={voucher.amount} theme={theme} />
                  }
                />
              </FadeIn>
            );
          })}
          {justBoughtVoucher && (
            <FadeIn
              orientation={'left'}
              delay={getDelay()}
              key={`NewlyBoughtVoucher`}
            >
              <VoucherCard
                theme={theme}
                left={
                  <CurrencySymbol
                    color={theme.baseColor}
                    isLoading={true}
                    isRequesting={true}
                    size={4}
                  />
                }
                center={
                  <>
                    <LoadingText
                      theme={theme}
                      children={t('dashboard.voucherShop.waitingFor')}
                      fontSize={2}
                    />
                    <LoadingText
                      theme={theme}
                      children={t('dashboard.voucherShop.voucher')}
                      fontSize={2}
                    />
                  </>
                }
                right={<></>}
              />
            </FadeIn>
          )}
        </VoucherContainer>
      ) : (
        <SubClaim>{t('dashboard.voucherShop.listNoVouchers')}</SubClaim>
      )}
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

const mapInfo = (
  providers: Array<VoucherProvider>,
  providerId: string
): VoucherProvider | undefined => {
  const provider = providers.find(provider => provider.id === providerId);
  return provider;
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const VoucherContainer = styled.div(() => [
  css`
    display: grid;
    grid-template-columns: 1fr;
    gap: 1rem;

    @media (min-width: 1000px) {
      grid-template-columns: 1fr 1fr;
    }
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
  provider: VoucherProvider | undefined;
  theme: Theme;
};

const VoucherContent = ({
  voucher,
  provider,
  theme,
}: VoucherContentProps): ReactElement => {
  return (
    <VoucherContentContainer theme={theme}>
      <VoucherText fontSize={1.25} theme={theme}>
        {provider ? provider.name : voucher.providerId}
      </VoucherText>
      <VoucherText fontSize={0.75} theme={theme}>
        {voucher.code}
      </VoucherText>
    </VoucherContentContainer>
  );
};

const VoucherText = styled.p<{ theme: Theme; fontSize: number }>(
  ({ theme, fontSize }) => [
    css`
      color: ${theme.textColorDark};
      font-size: ${fontSize}rem;
      margin: 0;
      padding: 0;
    `,
  ]
);

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
