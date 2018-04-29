module NBAStats.Types.Common where

import Prelude

newtype TeamID = TeamID Int

instance Show TeamID where
  show (TeamID id) = show id

-- Season is encoded by the start year
newtype Season = Season Int

-- |
-- Converts `Season 2017` into `"2017-18"`
instance Show Season where
  show (Season s) = (show s) ++ "-" ++ (show $ (s `mod` 100) + 1)
