module VoucherServer.GraphQLSchemas.GraphNode where

import Prelude

import GraphQL.Client.Args (type (==>))
import Type.Proxy (Proxy)

type Address = String

--------------------------------------------------------------------------------
-- Schema
--------------------------------------------------------------------------------
type Schema =
  { transfers ::
      { where ::
          { to :: Address
          }
      }
        ==> Array Transfer
  , notifications ::
      { where ::
          { safeAddress :: Address
          , transfer :: String
          }
      }
        ==> Array Notification
  }

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Transfer =
  { from :: Address
  , to :: Address
  , id :: String
  , amount :: String
  }

type Notification =
  { id :: String
  , transactionHash :: String
  , time :: String
  }

--------------------------------------------------------------------------------
-- Selectors
--------------------------------------------------------------------------------

selectors
  :: { transactionHash :: Proxy "transactionHash"
     , amount :: Proxy "amount"
     , prop :: Proxy "prop"
     , name :: Proxy "name"
     , from :: Proxy "from"
     , time :: Proxy "time"
     , id :: Proxy "id"
     , to :: Proxy "to"
     }
selectors = one
