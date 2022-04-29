import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "@circles-pink/web-client/src/onboarding";
import * as Steps from "generated/output/CirclesPink.Garden.StateMachine.Steps";
import { onboardingArgs } from "../onboardingArgs";

export default {
  title: "Components/Onboarding/Steps",
  component: Onboarding,
  argTypes: onboardingArgs,
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Onboarding>;

export const InfoGeneral = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.infoGeneral} />
);

export const AskUsername = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.askUsername} />
);

export const AskEmail = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.askEmail} />
);

export const InfoSecurity = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.infoSecurity} />
);

export const MagicWords = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.magicWords} />
);

export const Submit = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.submit} />
);
