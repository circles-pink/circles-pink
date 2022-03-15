import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { StepIndicator } from "circles/src/components/StepIndicator";

export default {
  title: "Components/StepIndicator",
  component: StepIndicator,
  argTypes: {
    // lang: {
    //   defaultValue: "en",
    //   options: ["en", "de"],
    //   control: { type: "inline-radio" },
    // },
    // initState: { control: { disable: true }, table: { disable: true } },
    steps: {
      defaultValue: [
        { label: "1" },
        { label: "2" },
        { label: "3" },
        { label: "4" },
        { label: "5" },
        { label: "6" },
      ],
      control: { disable: true },
    },
  },
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof StepIndicator>;

export const Default = (args): ReactElement => <StepIndicator {...args} />;
