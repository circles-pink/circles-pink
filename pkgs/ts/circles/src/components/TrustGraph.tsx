import React from 'react';

export const TrustGraph = () => {
  return <h2>Hello!</h2>;
};

// import React, { ReactElement, ReactNode, useEffect, useState } from 'react';
// import CytoscapeComponent from 'react-cytoscapejs';
// import Cytoscape, { LayoutOptions } from 'cytoscape';
// // @ts-ignore
// import COSEBilkent from 'cytoscape-cose-bilkent';
// import { TrustNode } from 'generated/output/CirclesCore';
// import { Address } from 'generated/output/Wallet.PrivateKey';

// // -----------------------------------------------------------------------------
// // Constants
// // -----------------------------------------------------------------------------

// const layout = {
//   name: 'cose-bilkent',
//   // other options
//   padding: 50,
//   nodeDimensionsIncludeLabels: true,
//   idealEdgeLength: 100,
//   edgeElasticity: 0.1,
//   animate: 'end',
//   animationDuration: 200,
//   //nodeRepulsion: 8500,
//   //randomize: true,
// };

// // -----------------------------------------------------------------------------
// // Utils
// // -----------------------------------------------------------------------------

// const getNode = (
//   key: Address,
//   value: Array<TrustNode>
// ): Cytoscape.ElementDefinition => ({
//   data: { id: key as unknown as string, label: key },
// });

// const getNodes = (data_: Graph): Cytoscape.ElementDefinition[] =>
//   Array.from(data_.entries()).map(([k, v]) => getNode(k, v));

// const getEdges = (data_: Graph): Cytoscape.ElementDefinition[] => [];

// const getElementsFromData = (data_: Graph): Cytoscape.ElementDefinition[] => [
//   ...getNodes(data_),
//   ...getEdges(data_),
// ];

// // -----------------------------------------------------------------------------
// // UI
// // -----------------------------------------------------------------------------

// export type Graph = Map<Address, Array<TrustNode>>;

// type TrustGraphProps = { graph: Graph };

// export const TrustGraph = ({ graph }: TrustGraphProps): ReactElement => {
//   const [cy, setCy] = React.useState<Cytoscape.Core | undefined>();

//   React.useEffect(() => {
//     if (!cy) return;
//     Cytoscape.use(COSEBilkent);
//     var layout_ = cy.layout(layout);
//     layout_.run();
//   }, [JSON.stringify(graph)]);

//   const elements = getElementsFromData(graph);

//   console.log(elements);

//   const stylesheets = [
//     {
//       selector: 'node',
//       style: {
//         width: 'label',
//         height: 'label',
//         padding: '8px',
//         shape: 'round-rectangle',
//         'background-color': 'red',
//         label: 'data(label)', // here you can label the nodes
//       } as any,
//     },
//     {
//       selector: 'node[label]',
//       style: {
//         label: 'data(label)',
//         'font-size': '20',
//         color: 'black',
//         'text-halign': 'center',
//         'text-valign': 'center',
//       },
//     },
//     {
//       selector: 'edge',
//       style: {
//         'curve-style': 'bezier',
//         'target-arrow-shape': 'triangle',
//         width: 1.5,
//       },
//     },
//   ];
//   return (
//     <CytoscapeComponent
//       cy={cy_ => {
//         if (!cy) setCy(cy_);
//       }}
//       elements={elements}
//       style={{
//         width: '100%',
//         height: '600px',
//         backgroundColor: 'rgba(249, 249, 245, 0.5)',
//         boxShadow: '0 0 4px 1px rgba(0,0,0,0.05)',
//       }}
//       layout={layout}
//       stylesheet={stylesheets}
//     />
//   );
// };
