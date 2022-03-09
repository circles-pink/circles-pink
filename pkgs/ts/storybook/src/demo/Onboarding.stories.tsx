import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "circles/src/onboarding";

export default {
  title: "Components/Onboarding",
  component: Onboarding,
  argTypes: {
    lang: {
      options: ["en", "de"],
      control: { type: "inline-radio" },
    },
  },
} as ComponentMeta<typeof Onboarding>;

export const GardenAPI = (args): ReactElement => (
  <Onboarding lang={args.lang} />
);
