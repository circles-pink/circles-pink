module CirclesPink.URI
  ( URI(..)
  , options
  , parse
  , print
  ) where

import Prelude
import Data.Either (Either, hush)
import Data.Newtype (class Newtype, unwrap, wrap)
import Text.Parsing.Parser (ParseError, Parser)
import Text.Parsing.Parser as P
import TypedEnv (class ParseValue)
import URI (AbsoluteURI, HierPath, Host, Path, Port, Query, UserInfo)
import URI.AbsoluteURI (AbsoluteURIOptions)
import URI.AbsoluteURI as U
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair

--------------------------------------------------------------------------------
newtype URI
  = URI (AbsoluteURI UserInfo (HostPortPair Host Port) Path HierPath Query)

instance parseValueURI :: ParseValue URI where
  parseValue = parse >>> hush

derive instance newtypeURI :: Newtype URI _

derive newtype instance showURI :: Show URI

--------------------------------------------------------------------------------
options :: Record (AbsoluteURIOptions UserInfo (HostPortPair Host Port) Path HierPath Query)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  }

parser :: Parser String URI
parser = U.parser options <#> wrap

parse :: String -> Either ParseError URI
parse s = P.runParser s parser

print :: URI -> String
print = unwrap >>> U.print options
