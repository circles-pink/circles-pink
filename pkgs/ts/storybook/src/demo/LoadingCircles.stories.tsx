import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { LoadingCircles } from "circles/src/components/LoadingCircles";

export default {
  title: "Components/LoadingCircles",
  component: LoadingCircles,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof LoadingCircles>;

export const Default = (args): ReactElement => <LoadingCircles {...args} />;
