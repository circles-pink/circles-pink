import React, { ReactElement } from "react";
import { ComponentMeta } from "@storybook/react";
import {
  TrustGraph,
  Graph,
} from "@circles-pink/web-client/src/components/TrustGraph";
import { TrustNode } from "@circles-pink/state-machine/output/CirclesCore";
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

const A = "0x984501180D63335928eA7fb59c17d33e0398Ed39";
const B = "0x23dfED77bAEC40b0fd64f74e14974681e7dD2498";
const C = "0x74e14974681e7dD249823dfED77bAEC40b0fd64f";
const D = "0xD63335928eA7fb59c17d33e0398Ed39984501180";
const E = "0xAEC40b0fd64f74e14974681e7dD249823dfED77b";
const F = "0x81e7dD249823dfED77bAEC40b0fd64f74e149746";

const nodeB: TrustNode = {
  isIncoming: true,
  isOutgoing: true,
  limitPercentageIn: 50,
  limitPercentageOut: 50,
  mutualConnections: [],
  safeAddress: unsafeAddrFromString(B),
};

const nodeA: TrustNode = {
  isIncoming: true,
  isOutgoing: true,
  limitPercentageIn: 50,
  limitPercentageOut: 50,
  mutualConnections: [],
  safeAddress: unsafeAddrFromString(A),
};

const nodeD: TrustNode = {
  isIncoming: true,
  isOutgoing: true,
  limitPercentageIn: 50,
  limitPercentageOut: 50,
  mutualConnections: [],
  safeAddress: unsafeAddrFromString(D),
};

const nodeE: TrustNode = {
  isIncoming: true,
  isOutgoing: true,
  limitPercentageIn: 50,
  limitPercentageOut: 50,
  mutualConnections: [],
  safeAddress: unsafeAddrFromString(E),
};

const graph: Graph = new Map([
  [unsafeAddrFromString(A), [nodeB, nodeE]],
  [unsafeAddrFromString(B), [nodeA]],
  [unsafeAddrFromString(C), [nodeA, nodeB] as TrustNode[]],
  [unsafeAddrFromString(E), [nodeD]],
  [unsafeAddrFromString(D), [nodeE]],
  [unsafeAddrFromString(F), [nodeA, nodeB, nodeD, nodeE] as TrustNode[]],
]);

export const Default = (args): ReactElement => <TrustGraph graph={graph} />;
