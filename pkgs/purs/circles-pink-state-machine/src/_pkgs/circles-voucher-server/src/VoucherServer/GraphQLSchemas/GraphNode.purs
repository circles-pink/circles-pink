module VoucherServer.GraphQLSchemas.GraphNode where

import Prelude

import GraphQL.Client.Args (type (==>))
import Type.Proxy (Proxy(..))

type Address = String

-- Schema
type Schema =
  { transfers ::
      { where ::
          { to :: Address
          }
      }
        ==> Array Transfer
  }

type Transfer =
  { from :: Address
  , to :: Address
  , id :: String
  , amount :: String
  }

prop = Proxy :: Proxy "prop"

name = Proxy :: Proxy "name"

from = Proxy :: Proxy "from"

to = Proxy :: Proxy "to"

id = Proxy :: Proxy "id"

amount = Proxy :: Proxy "amount"
