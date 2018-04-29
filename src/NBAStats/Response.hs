{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module NBAStats.Response where

import Prelude

import GHC.Generics

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as Text

-- |
-- This thing is basically a mirror of Aeson.Result a right now, but maybe we
-- will add more constructors later for richer error reporting.
data Response a
  = Success a
  | Error Text
  deriving (Show)

instance Functor Response where
  fmap f (Success a) = Success (f a)
  fmap _ (Error t) = Error t

instance Applicative Response where
  pure = Success

  (Success f) <*> r = fmap f r
  (Error t) <*> _r = Error t

instance Monad Response where
  (Success a) >>= f = f a
  (Error t) >>= _ = Error t 

  (Success _) >> k = k
  (Error t) >> _ = Error t

  return = Success
  fail = Error . Text.pack

fromJSON :: Aeson.FromJSON a => RawResult -> Response a
fromJSON RawResult{..} = case Aeson.fromJSON (Aeson.Array rowSet) of
  Aeson.Success a -> Success a
  Aeson.Error s -> Error (Text.pack s)

data RawResponse = RawResponse
  { resource :: Text
  , parameters :: Aeson.Object
  , resultSets :: [RawResult]
  } deriving (Generic, Show)

instance Aeson.FromJSON RawResponse

data RawResult = RawResult
  { name :: Text
  , headers :: [Text]
  , rowSet :: Vector Aeson.Value
  } deriving (Generic, Show)

instance Aeson.FromJSON RawResult

-- |
-- Helper to make it simpler to inspect raw JSON
rawResultMap :: RawResult -> Aeson.Object
rawResultMap RawResult{..} = HashMap.fromList $ zip headers (Vector.toList rowSet)
