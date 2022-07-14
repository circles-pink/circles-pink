import React, { ReactElement, useEffect, useState } from "react";
import { ComponentMeta } from "@storybook/react";
import { Debug } from "@circles-pink/web-client/src/onboarding/views/Debug";
import { useStateMachine } from "@circles-pink/web-client/src/onboarding/useStateMachine";
import { mkControl } from "@circles-pink/state-machine/output/CirclesPink.Garden.TS";
import { env } from "@circles-pink/web-client/src/env";
import { initDebug } from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State";
import { left } from "@circles-pink/state-machine/output/Data.FpTs.Either";

export default {
  title: "Components/Internal",
  component: Debug,
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Debug>;

const control = mkControl(env)({ extractEmail: left("foo@bar.com") });

export const DebugCirclesCore = (args): ReactElement => {
  const [state, act] = useStateMachine(initDebug, control);

  return (
    <>
      <Debug {...args} state={state} act={act} />
    </>
  );
};
