import React, { ReactElement, useEffect, useState } from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import Cytoscape, { LayoutOptions } from 'cytoscape';
import { Theme } from '../../context/theme';
import { ButtonRow, Margin } from '../helper';
import { Button } from '../forms';
import { JustText } from '../text';
import { t } from 'i18next';
import {
  Address,
  CirclesGraph,
  TrustConnection,
  TrustNode,
  TrustStateType,
  _Address,
  _Array,
  _Either,
  _Graph,
  _IxGraph,
  _Maybe,
  _Nullable,
  _Pair,
  _TrustConnection,
  _TrustNode,
  _TrustState,
  _UserIdent,
} from '@circles-pink/state-machine/src';

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
import { ordAddress } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { pipe } from 'fp-ts/lib/function';

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

const getNode = (tn: TrustNode): Cytoscape.ElementDefinition => ({
  data: {
    id: _Address.addrToString(_TrustNode.getAddress(tn)),
    label: _UserIdent.getIdentifier(tn.userIdent),
    isLoading: tn.isLoading,
  },
});

const getEdge = (tc: TrustConnection): Cytoscape.ElementDefinition => {
  const [pair, value] = _TrustConnection.unTrustConnection(
    pair => ts => [pair, ts] as const
  )(tc);

  const [source, target] = pipe(
    pair,
    _Pair.unPair(x1 => x2 => [x1, x2])
  );

  return {
    data: {
      source: _Address.addrToString(source),
      target: _Address.addrToString(target),
      value,
    },
  };
};

const getNodes = (
  data_: CirclesGraph
): readonly Cytoscape.ElementDefinition[] =>
  pipe(data_, _IxGraph.nodes(ordAddress), _Array.mapArray(getNode));

const getEdges = (
  graph: CirclesGraph
): readonly Cytoscape.ElementDefinition[] =>
  pipe(graph, _IxGraph.edges(ordAddress), _Array.mapArray(getEdge));

const getElementsFromData = (
  data_: CirclesGraph
): Cytoscape.ElementDefinition[] => [...getNodes(data_), ...getEdges(data_)];

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

type TrustGraphProps = {
  graph: CirclesGraph;
  expandTrustNetwork: (addr: Address) => void;
  theme: Theme;
};

(window as any).layouts = { concentric };

export const TrustGraph = ({
  graph,
  expandTrustNetwork,
  theme,
}: TrustGraphProps): ReactElement => {
  const [cy, setCy] = useState<Cytoscape.Core | undefined>();
  const [layout, setLayout] = useState<LayoutOptions>(concentric);

  const [prevGraph, setPrevGraph] = useState<CirclesGraph>(_Graph.empty);

  useEffect(() => {
    if (!cy) return;

    const diff = _Graph.getDiff(graph)(prevGraph);

    diff.forEach(instr => {
      pipe(
        instr
        // _Graph.unDiffInstruction({
        //   onAddNode: () => cy.add(),
        //   onDeleteNode: () => cy.remove(),
        // })
      );
    });

    setPrevGraph(graph);
  }, [graph]);

  const elements = getElementsFromData(graph);

  useEffect(() => {
    if (!cy) return;

    let loopAnimation1 =
      (address: Address) =>
      (ele: any): any => {
        const duration = 250 + Math.random() * 50;
        return ele
          .animation({
            style: {
              opacity: 1,
              'background-color': theme.baseColor,
              width: ele.data('label').length * 12,
              height: 15,
            },
            duration,
            easing: 'ease-in-out-sine',
          })
          .play()
          .promise('done')
          .then(() => loopAnimation2(address)(ele));
      };

    let loopAnimation2 =
      (address: Address) =>
      (ele: any): any => {
        const duration = 250 + Math.random() * 50;
        return ele
          .animation({
            style: {
              opacity: 1,
              'background-color': theme.lightColor,
              width: ele.data('label').length * 13,
              height: 20,
            },
            duration,
            easing: 'ease-in-out-sine',
          })
          .play()
          .promise('done')
          .then(() => loopAnimation1(address)(ele));
      };

    cy.on('tap', 'node', evt => {
      const addrStr: string = evt.target.id();
      const address = pipe(
        _Address.parseAddress(addrStr),
        _Nullable.toNullable
      );
      if (!address) return;
      expandTrustNetwork(address);
      loopAnimation1(address)(evt.target);
    });
  }, [cy]);

  useEffect(() => {
    if (!cy) return;

    let finishAnim = (ele: any): any => {
      const duration = 250 + Math.random() * 50;
      return ele
        .animation({
          style: {
            opacity: 1,
            'background-color': theme.baseColor,
            width: ele.data('label').length * 12,
            height: 15,
          },
          duration,
          easing: 'ease-in-out-sine',
        })
        .play();
    };

    setTimeout(() => {
      cy.nodes().map(x => {
        if (x.animated() && !x.data('isLoading')) {
          x.stop();
          finishAnim(x);
        }
        return x;
      });
    }, 1000);
  });

  const stylesheets = [
    {
      selector: 'node',
      style: {
        width: (node: any) => node.data('label').length * 12,
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
          const trustState = s._private.data.value;

          if (_TrustState.isTrusted(trustState)) {
            return 'solid';
          } else if (
            _TrustState.isLoadingTrust(trustState) ||
            _TrustState.isLoadingUntrust(trustState)
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
  if (_IxGraph.nodes(ordAddress)(graph).length === 0) return <></>;

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
