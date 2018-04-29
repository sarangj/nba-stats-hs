{-# LANGUAGE OverloadedStrings #-}
module NBAStats.Types.Coach where

import Prelude

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!))

data Coach = Coach
  { coach_id :: Text
  , coach_firstName :: Text
  , coach_lastName :: Text
  , coach_name :: Text
  , coach_code :: Text
  , coach_isAssistant :: Double
  , coach_type :: Text
  , coach_school :: Text
  } deriving (Show)

instance FromJSON Coach where
  parseJSON = withArray "Coach" $ \v -> Coach
    <$> parseJSON (v ! 2)
    <*> parseJSON (v ! 3)
    <*> parseJSON (v ! 4)
    <*> parseJSON (v ! 5)
    <*> parseJSON (v ! 6)
    <*> parseJSON (v ! 7)
    <*> parseJSON (v ! 8)
    <*> parseJSON (v ! 9)
