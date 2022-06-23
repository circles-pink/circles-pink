import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { CurrencySymbol } from "@circles-pink/web-client/src/components/CurrencySymbol";

export default {
  title: "Components/CurrencySymbol",
  component: CurrencySymbol,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof CurrencySymbol>;

export const Default = (args): ReactElement => (
  <CurrencySymbol
    color={args.color ? args.color : "hotpink"}
    isLoading={args.isLoading}
  />
);
