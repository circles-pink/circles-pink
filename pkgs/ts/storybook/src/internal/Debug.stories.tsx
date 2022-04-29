import React, { ReactElement, useEffect, useState } from "react";
import { ComponentMeta } from "@storybook/react";
import { Debug } from "@circles-pink/web-client/src/onboarding/views/Debug";
import { useStateMachine } from "@circles-pink/web-client/src/onboarding/useStateMachine";
import { mkControl } from "generated/output/CirclesPink.Garden.TS";
import { env } from "@circles-pink/web-client/src/env";
import { initDebug } from "generated/output/CirclesPink.Garden.StateMachine.State";

export default {
  title: "Components/Internal",
  component: Debug,
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Debug>;

const control = mkControl(env);

export const DebugCirclesCore = (args): ReactElement => {
  const [state, act] = useStateMachine(initDebug, control);

  return (
    <>
      <Debug {...args} state={state} act={act} />
    </>
  );
};
