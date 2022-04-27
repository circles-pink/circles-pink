import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { COMP } from "circles/src/components/COMP";

export default {
  title: "Components/COMP",
  component: COMP,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof COMP>;

export const Default = (args): ReactElement => <COMP {...args} />;
