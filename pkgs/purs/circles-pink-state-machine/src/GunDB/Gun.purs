-- | The `Gun` module provides a basic wrapper for gun.js database.
-- | see https://gun.eco for details about the gun database.
module GunDB
  ( GunChainCtx
  , GunDb
  , User
  , class Getable
  , each
  , filter
  , get
  , load
  , map
  , offline
  , on
  , once
  , put
  , set
  , syncWithPeer
  , syncWithPeers
  ) where

import Effect.Aff.Compat
import Control.Semigroupoid ((<<<))
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)

-- | Represents a reference to a gundb instance.
foreign import data GunDb :: Type

-- | Basic datastructure to chain gun operations like 'put', 'set' or 'once'
foreign import data GunChainCtx :: Type

-- | An authenticated user for a gundb instance
foreign import data User :: Type

-- | Creates a new gun database instance, and syncs the data with the given peer.
foreign import syncWithPeer :: String -> Effect GunDb

-- | Creates a new gun database instance, and syncs the data with the given peers.
foreign import syncWithPeers :: Array String -> Effect GunDb

-- | Creates a new local gun database instance, without syncing with any peers.
foreign import offline :: Effect GunDb

-- | A Typeclass which allows getting data from different sources.
-- |
-- | The `get` function takes a path either as a String or as an Array of Strings
-- | and returns a `GunChainCtx`.
-- | `get` can be called on a `GunDb` instance or a `User`
class Getable a b where
  get :: a -> b -> GunChainCtx

instance Getable String GunDb where
  get path db = _getOnGunDb [ path ] db

instance Getable (Array String) GunDb where
  get = _getOnGunDb

instance Getable String User where
  get path db = _getOnUser [ path ] db

instance Getable (Array String) User where
  get = _getOnUser

foreign import _getOnGunDb :: Array String -> GunDb -> GunChainCtx

foreign import _getOnUser :: Array String -> User -> GunChainCtx

-- | Save data into gun, syncing it with your connected peers.
foreign import put :: Json -> GunChainCtx -> Effect GunChainCtx

-- | Get the current data without subscribing to updates. Or `Nothing` if it cannot be found.
once :: GunChainCtx -> Aff (Maybe { data :: Json, key :: Json })
once = fromEffectFnAff <<< _once Nothing Just

foreign import _once :: forall a. Maybe a -> (a -> Maybe a) -> GunChainCtx -> EffectFnAff (Maybe { data :: Json, key :: Json })

-- | Loads a complete graph at once
load :: Int -> GunChainCtx -> Aff (Maybe Json)
load waittime ctx = fromEffectFnAff (_load Nothing Just waittime ctx)

foreign import _load :: forall a. Maybe a -> (a -> Maybe a) -> Int -> GunChainCtx -> EffectFnAff (Maybe Json)

-- | Add a unique item to an unordered list.
foreign import set :: GunChainCtx -> GunChainCtx -> Effect GunChainCtx

-- | Map iterates over each property and item on a node, passing it down the chain,
-- | transforming the data with the given function. It also subscribes to every item as well
-- | and listens for newly inserted items.
foreign import map :: (Foreign -> Foreign) -> GunChainCtx -> GunChainCtx

-- | Filter iterates over each property and item on a node, passing it down the chain,
-- | filtering the data with the given function. It also subscribes to every item as well
-- | and listens for newly inserted items.
foreign import filter :: (Foreign -> Boolean) -> GunChainCtx -> GunChainCtx

-- | Each iterates over each property and item on a node, passing it down the chain.
-- | It also subscribes to every item as well
-- | and listens for newly inserted items.
foreign import each :: GunChainCtx -> GunChainCtx

-- | Subscribe to updates and changes on a node or property in realtime.
on :: GunChainCtx -> Aff { data :: Foreign, key :: Foreign }
on = fromEffectFnAff <<< _on

foreign import _on :: GunChainCtx -> EffectFnAff { data :: Foreign, key :: Foreign }
