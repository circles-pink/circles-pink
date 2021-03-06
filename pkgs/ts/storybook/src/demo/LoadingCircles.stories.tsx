import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { LoadingCircles } from "@circles-pink/web-client/src/components/LoadingCircles";

export default {
  title: "Components/LoadingCircles",
  component: LoadingCircles,
  argTypes: {
    duration: { control: { type: "range", min: 0.1, max: 10, step: 0.01 } },
    width: { control: { type: "range", min: 0, max: 300, step: 1 } },
    maxScale: { control: { type: "range", min: 0.5, max: 1.5, step: 0.01 } },
    pulse: { control: { type: "range", min: 0, max: 1.0, step: 0.01 } },
  },
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof LoadingCircles>;

export const Default = (args): ReactElement => <LoadingCircles {...args} />;
