import React, { ReactElement, ReactNode, useEffect, useState } from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import Cytoscape, {
  CircleLayoutOptions,
  CoseLayoutOptions,
  LayoutOptions,
} from 'cytoscape';
// import COSEBilkent from 'cytoscape-cose-bilkent';
import { TrustNode } from '@circles-pink/state-machine/output/CirclesCore';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

const layout: Partial<CircleLayoutOptions> & Pick<CircleLayoutOptions, 'name'> =
  {
    name: 'circle',
    animate: true,
    // other options
    // padding: 75,
    // nodeDimensionsIncludeLabels: true,
    // idealEdgeLength: n => 100,
    // edgeElasticity: 0.1,
    animationDuration: 550,
    // refresh: 1,
    // randomize: false,
    // componentSpacing: 7000,
    // nodeRepulsion: () => 2000,
  };

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

const getNode = (
  key: Address,
  value: Array<TrustNode>
): Cytoscape.ElementDefinition => ({
  data: {
    id: key as unknown as string,
    label: (key as unknown as string).substring(0, 6),
  },
});

const getEdge = (
  source: Address,
  value: TrustNode[]
): Cytoscape.ElementDefinition[] =>
  value.map(target => ({
    data: {
      source,
      target: target.safeAddress,
    },
  }));

const getNodes = (data_: Graph): Cytoscape.ElementDefinition[] =>
  Array.from(data_.entries()).map(([k, v]) => getNode(k, v));

const getEdges = (data_: Graph): Cytoscape.ElementDefinition[] =>
  Array.from(data_.entries()).flatMap(([k, v]) => getEdge(k, v));

const getElementsFromData = (data_: Graph): Cytoscape.ElementDefinition[] => [
  ...getNodes(data_),
  ...getEdges(data_),
];

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

export type Graph = Map<Address, Array<TrustNode>>;

type TrustGraphProps = { graph: Graph };

export const TrustGraph = ({ graph }: TrustGraphProps): ReactElement => {
  const [cy, setCy] = React.useState<Cytoscape.Core | undefined>();
  // const [cyInitialized, setCyInitialized] = React.useState<boolean>(false);

  React.useEffect(() => {
    if (!cy) return;
    var layout_ = cy.layout(layout);
    layout_.run();
  }, [JSON.stringify(graph)]);

  // React.useEffect(() => {
  //   console.log(COSEBilkent);
  //   Cytoscape.use(COSEBilkent);
  //   setCyInitialized(true);
  // }, []);

  const elements = getElementsFromData(graph);

  console.log(elements);

  const stylesheets = [
    {
      selector: 'node',
      style: {
        width: 'label',
        height: 'label',
        padding: '4px',
        shape: 'round-rectangle',
        'background-color': 'red',
        label: 'data(label)', // here you can label the nodes
      } as any,
    },
    {
      selector: 'node[label]',
      style: {
        label: 'data(label)',
        'font-size': '20',
        color: 'black',
        'text-halign': 'center',
        'text-valign': 'center',
      },
    },
    {
      selector: 'edge',
      style: {
        'curve-style': 'bezier',
        'target-arrow-shape': 'triangle',
        width: 1,
      },
    },
  ];

  // if (!cyInitialized) return <></>;

  return (
    <CytoscapeComponent
      cy={cy_ => {
        if (!cy) setCy(cy_);
      }}
      elements={elements}
      style={{
        width: '100%',
        height: '600px',
        backgroundColor: 'rgba(249, 249, 245, 0.5)',
        boxShadow: '0 0 4px 1px rgba(0,0,0,0.05)',
      }}
      layout={layout}
      stylesheet={stylesheets}
    />
  );
};
