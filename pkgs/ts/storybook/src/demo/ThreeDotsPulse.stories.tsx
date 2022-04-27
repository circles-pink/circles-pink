import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { ThreeDotsPulse } from "circles/src/components/ThreeDotsPulse";

export default {
  title: "Components/ThreeDotsPulse",
  component: ThreeDotsPulse,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof ThreeDotsPulse>;

export const Default = (args): ReactElement => <ThreeDotsPulse {...args} />;
