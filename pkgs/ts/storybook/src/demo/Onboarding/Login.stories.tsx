import React, { ReactElement, useEffect, useState } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "@circles-pink/web-client/src/onboarding";
import { onboardingArgs } from "../onboardingArgs";
import { initLogin } from "generated/output/CirclesPink.Garden.StateMachine.State";

export default {
  title: "Components/Onboarding/Login",
  component: Onboarding,
  argTypes: onboardingArgs,
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Onboarding>;

export const GardenLogin = (args): ReactElement => {
  const [content, setContent] = useState<{}>();

  // useEffect(() => {
  //   getContent({ endpoint: process.env.STORYBOOK_DIRECTUS_URL }).then(
  //     setContent
  //   );
  // }, []);

  return <Onboarding content={content} {...args} initState={initLogin} />;
};
