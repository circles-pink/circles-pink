export const cise = {
  // -------- Mandatory parameters --------
  name: 'cise',

  // ClusterInfo can be a 2D array contaning node id's or a function that returns cluster ids.
  // For the 2D array option, the index of the array indicates the cluster ID for all elements in
  // the collection at that index. Unclustered nodes must NOT be present in this array of clusters.
  //
  // For the function, it would be given a Cytoscape node and it is expected to return a cluster id
  // corresponding to that node. Returning negative numbers, null or undefined is fine for unclustered
  // nodes.
  // e.g
  // Array:                                     OR          function(node){
  //  [ ['n1','n2','n3'],                                       ...
  //    ['n5','n6']                                         }
  //    ['n7', 'n8', 'n9', 'n10'] ]
  clusters: function (node: any) {
    return node.data.id;
  },

  // -------- Optional parameters --------
  // Whether to animate the layout
  // - true : Animate while the layout is running
  // - false : Just show the end result
  // - 'end' : Animate directly to the end result
  animate: false,

  // number of ticks per frame; higher is faster but more jerky
  refresh: 10,

  // Animation duration used for animate:'end'
  animationDuration: undefined,

  // Easing for animate:'end'
  animationEasing: undefined,

  // Whether to fit the viewport to the repositioned graph
  // true : Fits at end of layout for animate:false or animate:'end'
  fit: true,

  // Padding in rendered co-ordinates around the layout
  padding: 30,

  // separation amount between nodes in a cluster
  // note: increasing this amount will also increase the simulation time
  nodeSeparation: 12.5,

  // Inter-cluster edge length factor
  // (2.0 means inter-cluster edges should be twice as long as intra-cluster edges)
  idealInterClusterEdgeLengthCoefficient: 1.4,

  // Whether to pull on-circle nodes inside of the circle
  allowNodesInsideCircle: false,

  // Max percentage of the nodes in a circle that can move inside the circle
  maxRatioOfNodesInsideCircle: 0.1,

  // - Lower values give looser springs
  // - Higher values give tighter springs
  springCoeff: 0.45,

  // Node repulsion (non overlapping) multiplier
  nodeRepulsion: 4500,

  // Gravity force (constant)
  gravity: 0.25,

  // Gravity range (constant)
  gravityRange: 3.8,

  // Layout event callbacks; equivalent to `layout.one('layoutready', callback)` for example
  ready: function () {}, // on layoutready
  stop: function () {}, // on layoutstop
};
