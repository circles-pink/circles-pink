module Data.Graph.Lens where

import Prelude

import Data.Either (hush)
import Data.Graph (Graph)
import Data.Graph as G
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromJust)
import Data.Pair (Pair)
import Partial.Unsafe (unsafePartial)

node :: forall id e n. Ord id => id -> Lens' (Graph id e n) (Maybe n)
node id = lens getter setter
  where
  getter g = G.lookupNode id g # hush
  setter g Nothing = unsafePartial (G.deleteNode id g # hush # fromJust)
  setter g (Just n) = unsafePartial (G.addNode id n g # hush # fromJust)

edge :: forall id e n. Ord id => Pair id -> Lens' (Graph id e n) (Maybe e)
edge conn = lens getter setter
  where
  getter g = G.lookupEdge conn g # hush
  setter g Nothing = unsafePartial (G.deleteEdge conn g # hush # fromJust)
  setter g (Just n) = unsafePartial (G.addEdge conn n g # hush # fromJust)

