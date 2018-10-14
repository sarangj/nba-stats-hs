{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module NBAStats.Endpoint where

import Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as C8
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.URI as HTTP

import NBAStats.Response

data Endpoint a b = Endpoint
  { endpoint_resourceName :: Text
  , endpoint_queryBuilder :: a -> HTTP.Query
  , endpoint_responseHandler :: RawResponse -> Response b
  }

get :: Endpoint a b -> a -> IO (Response b)
get Endpoint{..} input = fmap ((=<<) endpoint_responseHandler) raw
  where
    query = endpoint_queryBuilder input
    raw = getRaw endpoint_resourceName query

getRaw
  :: Text
  -> HTTP.Query
  -> IO (Response RawResponse)
getRaw resourceName query = fmap httpToResponse $ HTTP.httpLBS $
  HTTP.setRequestHeaders defaultHeaders $
  HTTP.setRequestQueryString query $
  HTTP.setRequestPath path $
  HTTP.setRequestMethod "GET" $
  HTTP.setRequestHost "stats.nba.com"
  HTTP.defaultRequest
  where
    endpointBS = Text.encodeUtf8 resourceName
    path = "/stats/" <> endpointBS
    httpToResponse = maybe (Error "Invalid JSON") Success . Aeson.decode . HTTP.getResponseBody

getRaw'
  :: Text
  -> HTTP.Query
  -> IO Aeson.Object
getRaw' resourceName query = fmap HTTP.getResponseBody $ HTTP.httpJSON $
  HTTP.setRequestHeaders defaultHeaders $
  HTTP.setRequestQueryString query $
  HTTP.setRequestPath path $
  HTTP.setRequestMethod "GET" $
  HTTP.setRequestHost "stats.nba.com"
  HTTP.defaultRequest
  where
    endpointBS = Text.encodeUtf8 resourceName
    path = "/stats/" <> endpointBS

toQueryItem :: Show a => C8.ByteString -> a -> HTTP.QueryItem
toQueryItem key s = (key, Just (C8.pack (show s)))

defaultHeaders :: [(HTTP.HeaderName, C8.ByteString)]
defaultHeaders =
  [ ("accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8")
  , ("user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_2_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36")
  , ("Dnt", "1")
  , ("Accept-Encoding", "gzip, deflate")
  , ("Accept-Language", "en-US,en;q=0.9")
  ]
