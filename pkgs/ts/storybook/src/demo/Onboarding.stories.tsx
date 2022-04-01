import React, { ReactElement, useEffect, useState } from "react";
import { ComponentMeta } from "@storybook/react";
import { Onboarding } from "circles/src/onboarding";
import { onboardingArgs } from "./onboardingArgs";
import { getContent } from "@circles-pink/content/src";

export default {
  title: "Components/Onboarding",
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

  useEffect(() => {
    getContent({ endpoint: process.env.STORYBOOK_DIRECTUS_URL }).then(
      setContent
    );
  }, []);

  return (
    <>
      <Onboarding content={content} {...args} />
    </>
  );
};
