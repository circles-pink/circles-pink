import React from "react";
import { Frame } from "@circles-pink/web-client/src/onboarding";

export const parameters = {
  actions: { argTypesRegex: "^on[A-Z].*" },
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
};

export const decorators = [
  (Story) => (
    <Frame>
      <Story />
    </Frame>
  ),
];
