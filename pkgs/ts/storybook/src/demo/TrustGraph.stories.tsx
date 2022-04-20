import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { TrustGraph, Graph } from "circles/src/components/TrustGraph";
import { TrustNode } from "generated/output/CirclesCore";
import { unsafeAddrFromString } from "generated/output/Wallet.PrivateKey";
// import Cytoscape from "cytoscape";
// import COSEBilkent from "cytoscape-cose-bilkent";

// Cytoscape.use(COSEBilkent);

export default {
  title: "Components/TrustGraph",
  component: TrustGraph,
  argTypes: {},
  parameters: {
    previewTabs: {
      "storybook/docs/panel": { hidden: true },
    },
  },
} as ComponentMeta<typeof TrustGraph>;

const graph: Graph = new Map([
  [unsafeAddrFromString("A"), [] as TrustNode[]],
  [unsafeAddrFromString("B"), [] as TrustNode[]],
  [unsafeAddrFromString("C"), [] as TrustNode[]],
]);

export const Default = (args): ReactElement => <TrustGraph graph={graph} />;
