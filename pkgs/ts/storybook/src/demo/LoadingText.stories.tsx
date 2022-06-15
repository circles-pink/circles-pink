import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { LoadingText } from "@circles-pink/web-client/src/components/text";
import { defaultTheme } from "@circles-pink/web-client/src/context/theme";

export default {
  title: "Components/Text/LoadingText",
  component: LoadingText,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof LoadingText>;

export const Default = (args): ReactElement => (
  <LoadingText theme={defaultTheme} fontSize={5} {...args}>Hello World</LoadingText>
);
