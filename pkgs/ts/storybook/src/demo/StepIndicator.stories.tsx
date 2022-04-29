import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { StepIndicator } from "@circles-pink/web-client/src/components/StepIndicator";

export default {
  title: "Components/StepIndicator",
  component: StepIndicator,
  argTypes: {
    selected: {
      defaultValue: 2,
    },
    steps: {
      defaultValue: [
        { label: "1" },
        { label: "2" },
        { label: "3" },
        { label: "4" },
        { label: "5" },
        { label: "6" },
        { label: "7" },
      ],
      control: { disable: true },
    },
    speed: {
      control: { type: "range", min: 0, max: 0.01, step: 0.0001 },
    },
  },
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof StepIndicator>;

export const Default = (args): ReactElement => <StepIndicator {...args} />;
