import React, { ReactElement, useEffect, useState } from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import Cytoscape, { LayoutOptions } from 'cytoscape';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { Theme } from '../../context/theme';
import { ButtonRow, Margin } from '../helper';
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
import { JustText } from '../text';
import { t } from 'i18next';
import { TrustNode } from '@circles-pink/state-machine/output/CirclesPink.Data.TrustNode';

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

const getNode = (
  key: Address,
  value: TrustNode
): Cytoscape.ElementDefinition => ({
  data: {
    id: addrToString(key),
    label: getIdentifier(TN.unwrap(value).userIdent),
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

    // let loopAnimation1 = (ele: any): any => {
    //   const duration = 250 + Math.random() *  50
    //   return ele
    //     .animation({
    //       style:  { opacity: 1, 'background-color': theme.baseColor, width: ele.data('label').length * 12, height: 15}, 
    //       duration,
    //       easing: 'ease-in-out-sine',
    //     })
    //     .play()
    //     .promise('done')
    //     .then(() => {
    //       loopAnimation2(ele);
    //       console.log('done');
    //     });
    // };

    // let loopAnimation2 = (ele: any): any => {
    //   const duration = 250 + Math.random() * 50
    //   return ele
    //     .animation({
    //       style: { opacity: 1, 'background-color': theme.lightColor, width: ele.data('label').length * 13, height: 20},
    //       duration,
    //       easing: 'ease-in-out-sine',
    //     })
    //     .play()
    //     .promise('done')
    //     .then(() => {
    //       loopAnimation1(ele);
    //       console.log('done');
    //     });
    // };

    cy.on('tap', 'node', evt => {
      expandTrustNetwork(evt.target.id());
      //loopAnimation1(evt.target);

      // evt.cy.nodes().animate({
      //   style: {}
      // })
    });
  }, [cy]);

  const stylesheets = [
    {
      selector: 'node',
      style: {
        width: (node:any) => node.data('label').length * 12 ,
        height: 15,
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

      <Margin top={0.75} bottom={0.75}>
        <JustText fontSize={1.25}>{t('trustGraph.switchLayout')}</JustText>
      </Margin>

      <ButtonRow>
        <Button
          theme={theme}
          prio={layout.name === 'concentric' ? 'medium' : 'low'}
          onClick={() => setLayout(concentric)}
        >
          {t('trustGraph.layoutOptions.circles')}
        </Button>
        <Button
          theme={theme}
          prio={layout.name === 'cise' ? 'medium' : 'low'}
          onClick={() => setLayout(cise)}
        >
          {t('trustGraph.layoutOptions.cise')}
        </Button>
        <Button
          theme={theme}
          prio={layout.name === 'cose-bilkent' ? 'medium' : 'low'}
          onClick={() => setLayout(coseBilkent)}
        >
          {t('trustGraph.layoutOptions.cose')}
        </Button>
      </ButtonRow>
    </>
  );
};
