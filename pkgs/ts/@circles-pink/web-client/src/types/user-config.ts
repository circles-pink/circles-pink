export type UserConfig = {
  email?: string | ((email: string) => void);
  onTrackingEvent?: (json: unknown) => void;
  onTrackingResumee?: (json: unknown) => void;
  voucherShopEnabled: boolean;
};
