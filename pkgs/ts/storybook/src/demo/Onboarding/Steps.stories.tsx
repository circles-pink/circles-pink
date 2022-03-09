import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "circles/src/onboarding";
import * as Steps from "generated/output/CirclesPink.Garden.StateMachine.Steps";

export default {
  title: "Components/Onboarding/Steps",
  component: Onboarding,
  argTypes: {
    initState: { control: { disable: true }, table: { disable: true } },
    lang: {
      defaultValue: "en",
      options: ["en", "de"],
      control: { type: "inline-radio" },
    },
  },
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Onboarding>;

export const InfoGeneral = (args): ReactElement => (
  <Onboarding lang={args.lang} initState={Steps.infoGeneral} />
);

export const AskUsername = (args): ReactElement => (
  <Onboarding lang={args.lang} initState={Steps.askUsername} />
);

export const AskEmail = (args): ReactElement => (
  <Onboarding lang={args.lang} initState={Steps.askEmail} />
);

export const InfoSecurity = (args): ReactElement => (
  <Onboarding lang={args.lang} initState={Steps.infoSecurity} />
);

export const MagicWords = (args): ReactElement => (
  <Onboarding lang={args.lang} initState={Steps.magicWords} />
);
