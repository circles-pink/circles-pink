module VoucherServer.GraphQLSchemas.GraphNode where

import Prelude

import CirclesPink.Data.Address (Address)
import Data.BN (BN)
import GraphQL.Client.Args (type (==>))

-- Schema
type Schema =
  { transfers ::
      { where ::
          { from :: Address
          , to :: Address
          }
      }
        ==> Array Transfer
  }

type Transfer =
  { from :: Address
  , to :: Address
  , id :: String
  , amount :: BN
  }