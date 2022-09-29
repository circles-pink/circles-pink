import { ConcentricLayoutOptions } from 'cytoscape';

export const concentric: Partial<ConcentricLayoutOptions> &
  Pick<ConcentricLayoutOptions, 'name'> = {
  name: 'concentric',
  animate: true,
  fit: true,
  // other options
  // padding: 75,
  // nodeDimensionsIncludeLabels: true,
  // idealEdgeLength: n => 100,
  // edgeElasticity: 0.1,
  animationDuration: 550,
  // refresh: 1,
  // randomize: true,
  // componentSpacing: 7000,
  // nodeRepulsion: () => 2000,
};
