import React, { ReactElement, useEffect, useState } from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import Cytoscape, { LayoutOptions } from 'cytoscape';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import {
  addrToString,
  Graph,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import {
  getIdentifier,
  UserIdent,
} from '@circles-pink/state-machine/output/CirclesPink.Data.UserIdent';
import {
  isLoadingTrust,
  isLoadingUntrust,
  isTrusted,
  TrustState,
} from '@circles-pink/state-machine/output/CirclesPink.Data.TrustState';
import { toFpTsPair, toFpTsTuple } from '../../utils/fpTs';
import { Theme } from '../../context/theme';

import { ButtonRow } from '../helper';
import { Button } from '../forms';

// -----------------------------------------------------------------------------
// Layouts
// -----------------------------------------------------------------------------

import COSEBilkent from 'cytoscape-cose-bilkent';
import CISE from 'cytoscape-cise';

Cytoscape.use(CISE);
Cytoscape.use(COSEBilkent);

import { cise } from './layout/cise';
import { coseBilkent } from './layout/coseBilkent';
import { concentric } from './layout/concentric';

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
    value,
  },
});

const getNodes = (data_: Graph): Cytoscape.ElementDefinition[] =>
  data_.nodes.map(n => {
    const [key, value] = toFpTsTuple(n);
    return getNode(key, value);
  });

const getEdges = (data_: Graph): Cytoscape.ElementDefinition[] =>
  data_.edges.map(n => {
    const [conn, value] = toFpTsTuple(n);
    const [from, to] = toFpTsPair(conn);
    return getEdge(from, to, value.trustState);
  });

const getElementsFromData = (data_: Graph): Cytoscape.ElementDefinition[] => [
  ...getNodes(data_),
  ...getEdges(data_),
];

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

type TrustGraphProps = {
  graph: Graph;
  expandTrustNetwork: (addr: string) => void;
  theme: Theme;
};

export const TrustGraph = ({
  graph,
  expandTrustNetwork,
  theme,
}: TrustGraphProps): ReactElement => {
  const [cy, setCy] = useState<Cytoscape.Core | undefined>();
  const [layout, setLayout] = useState<LayoutOptions>(concentric);

  const elements = getElementsFromData(graph);

  useEffect(() => {
    if (!cy) return;
    const layout_ = cy.layout(layout);
    layout_.run();
  }, [JSON.stringify(elements)]);

  useEffect(() => {
    if (!cy) return;
    cy.on('tap', 'node', evt => {
      expandTrustNetwork(evt.target.id());
    });
  }, [cy]);

  const stylesheets = [
    {
      selector: 'node',
      style: {
        width: 'label',
        height: 'label',
        padding: '4px',
        shape: 'round-rectangle',
        'background-color': theme.baseColor,
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
        'curve-style': 'unbundled-bezier',
        'target-arrow-shape': 'triangle',
        width: 1,
        'line-style': (s: any) => {
          const trustState = s._private.data.value as TrustState;

          if (isTrusted(trustState)) {
            return 'solid';
          } else if (
            isLoadingTrust(trustState) ||
            isLoadingUntrust(trustState)
          ) {
            return 'dotted';
          } else {
            return 'dashed';
          }
        },
      },
    },
  ];

  // Preventing errors in some layouts for an empty graph
  if (graph.nodes.length === 0) return <></>;

  return (
    <>
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
      <br />
      <ButtonRow>
        <Button
          theme={theme}
          prio={layout.name === 'concentric' ? 'medium' : 'low'}
          onClick={() => setLayout(concentric)}
        >
          Circles
        </Button>
        <Button
          theme={theme}
          prio={layout.name === 'cise' ? 'medium' : 'low'}
          onClick={() => setLayout(cise)}
        >
          Cise
        </Button>
        <Button
          theme={theme}
          prio={layout.name === 'cose-bilkent' ? 'medium' : 'low'}
          onClick={() => setLayout(coseBilkent)}
        >
          Cose
        </Button>
      </ButtonRow>
    </>
  );
};
