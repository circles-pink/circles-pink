import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import { TrustGraph, Graph } from "circles/src/components/TrustGraph";
import { TrustNode } from "generated/output/CirclesCore";
import {
  Address,
  unsafeAddrFromString,
} from "generated/output/Wallet.PrivateKey";
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

const nodeB: TrustNode = {
  isIncoming: true,
  isOutgoing: true,
  limitPercentageIn: 50,
  limitPercentageOut: 50,
  mutualConnections: [],
  safeAddress: unsafeAddrFromString("B"),
};

const nodeA: TrustNode = {
  isIncoming: true,
  isOutgoing: true,
  limitPercentageIn: 50,
  limitPercentageOut: 50,
  mutualConnections: [],
  safeAddress: unsafeAddrFromString("A"),
};

const graph: Graph = new Map([
  [unsafeAddrFromString("A"), [nodeB]],
  [unsafeAddrFromString("B"), [nodeA]],
  [unsafeAddrFromString("C"), [nodeA, nodeB] as TrustNode[]],
]);

export const Default = (args): ReactElement => <TrustGraph graph={graph} />;
