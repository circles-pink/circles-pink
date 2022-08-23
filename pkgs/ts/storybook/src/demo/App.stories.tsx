import React, { ReactElement, useEffect, useState } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "@circles-pink/web-client/src/onboarding";
import { onboardingArgs } from "./onboardingArgs";
import { getContent } from "@circles-pink/content/src";
import { initLanding } from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State";

export default {
  title: "Components/App",
  component: Onboarding,
  argTypes: onboardingArgs,
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Onboarding>;

export const GardenAPI = (args): ReactElement => {
  const [content, setContent] = useState<{}>();
  const [trackingResumee, setTrackingResumee] = useState<unknown>();

  useEffect(() => {
    console.log("Tracking Resumee", JSON.stringify(trackingResumee, null, 2));
  }, [trackingResumee]);

  return (
    <Onboarding
      {...args}
      initState={initLanding}
      content={content}
      onTrackingEvent={(te) => console.log("Tracking Event", te)}
      onTrackingResumee={(updateResumee) => {
        setTrackingResumee(updateResumee);
      }}
    />
  );
};

export const TestEnv = (args): ReactElement => {
  const [content, setContent] = useState<{}>();

  return (
    <>
      <Onboarding content={content} {...args} initState={initLanding} testEnv />
    </>
  );
};
