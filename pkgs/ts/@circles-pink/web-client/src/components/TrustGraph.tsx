import React, { ReactElement, ReactNode, useEffect, useState } from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import Cytoscape, {
  CircleLayoutOptions,
  CoseLayoutOptions,
  LayoutOptions,
} from 'cytoscape';
// import COSEBilkent from 'cytoscape-cose-bilkent';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import {
  addrToString,
  Graph,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import {
  getIdentifier,
  UserIdent,
} from '@circles-pink/state-machine/output/CirclesPink.Data.UserIdent';
import { TrustState } from '@circles-pink/state-machine/output/CirclesPink.Data.TrustState';
import { toFpTsTuple } from '../utils/fpTs';

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
  value: UserIdent
): Cytoscape.ElementDefinition => ({
  data: {
    id: addrToString(key),
    label: getIdentifier(value),
  },
});

const getEdge = (
  source: Address,
  target: Address,
  value: TrustState
): Cytoscape.ElementDefinition => ({
  data: {
    source: addrToString(source),
    target: addrToString(target),
  },
});

const getNodes = (data_: Graph): Cytoscape.ElementDefinition[] =>
  data_.nodes.map(n => {
    const [key, value] = toFpTsTuple(n);
    return getNode(key, value);
  });

const getEdges = (data_: Graph): Cytoscape.ElementDefinition[] =>
  data_.edges.map(n => {
    const [from, vt] = toFpTsTuple(n);
    const [to, value] = toFpTsTuple(vt);
    return getEdge(from, to, value);
  });

const getElementsFromData = (data_: Graph): Cytoscape.ElementDefinition[] => [
  ...getNodes(data_),
  ...getEdges(data_),
];

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

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
