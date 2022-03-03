import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "circles/src/onboarding/Onboarding";
import * as Steps from "generated/output/CirclesPink.Garden.StateMachine.Steps";

export default {
  title: "Components/Onboarding/Steps",
  component: Onboarding,
} as ComponentMeta<typeof Onboarding>;

export const InfoGeneral = (): ReactElement => (
  <Onboarding initState={Steps.infoGeneral} />
);

export const AskUsername = (): ReactElement => (
  <Onboarding initState={Steps.askUsername} />
);

export const AskEmail = (): ReactElement => (
  <Onboarding initState={Steps.askEmail} />
);

export const InfoSecurity = (): ReactElement => (
  <Onboarding initState={Steps.infoSecurity} />
);

export const MagicWords = (): ReactElement => (
  <Onboarding initState={Steps.magicWords} />
);
