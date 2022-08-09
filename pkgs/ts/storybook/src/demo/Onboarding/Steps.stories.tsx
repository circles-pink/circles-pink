import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "@circles-pink/web-client/src/onboarding";
import * as Steps from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Steps";
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

const userConfig = {
  onTrackingEvent: (te) => console.log("Tracking Event", te),
};

export const InfoGeneral = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.infoGeneral} userConfig={userConfig}/>
);

export const AskUsername = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.askUsername} userConfig={userConfig} />
);

export const AskEmail = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.askEmail} userConfig={userConfig} />
);

export const InfoSecurity = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.infoSecurity} userConfig={userConfig} />
);

export const MagicWords = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.magicWords} userConfig={userConfig} />
);

export const Submit = (args): ReactElement => (
  <Onboarding {...args} initState={Steps.submit} userConfig={userConfig} />
);
