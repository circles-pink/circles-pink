import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { Button } from "@circles-pink/web-client/src/components/forms";

export default {
  title: "Components/Forms/Button",
  component: Button,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Button>;

export const Default = (args): ReactElement => <Button {...args} />;
