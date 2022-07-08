import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import {
  Field,
  GridRow,
} from "@circles-pink/web-client/src/components/GridRow";

export default {
  title: "Components/Grid/GridRow",
  component: GridRow,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof GridRow>;

const fields: Field[] = [
  {
    width: 1,
    content: <h2>Hello</h2>,
    align: "LEFT",
  },
  {
    width: 1,
    content: "World",
    align: "LEFT",
  },
];

export const Default = (args): ReactElement => (
  <GridRow {...args} fields={fields} />
);
