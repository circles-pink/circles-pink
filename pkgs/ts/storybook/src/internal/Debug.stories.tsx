import React, { ReactElement, useEffect, useState } from "react";
import { ComponentMeta } from "@storybook/react";
import { Debug } from "@circles-pink/web-client/src/onboarding/views/Debug";
import { useStateMachine } from "@circles-pink/web-client/src/onboarding/useStateMachine";
import { mkCfg } from "@circles-pink/web-client/src/onboarding";
import { _StateMachine, _TS } from "@circles-pink/state-machine/src";
import { env } from "@circles-pink/web-client/src/env";
import { fromFetchImplNative } from '@circles-pink/web-client/src/safe-as';

export default {
  title: "Components/Internal",
  component: Debug,
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof Debug>;

const cfg = mkCfg({})

const control = _TS.mkControl(fromFetchImplNative(window.fetch))(env)(cfg);

export const DebugCirclesCore = (args): ReactElement => {
  const [state, act] = useStateMachine(initDebug, control);

  return (
    <>
      <Debug {...args} state={state} act={act} />
    </>
  );
};
