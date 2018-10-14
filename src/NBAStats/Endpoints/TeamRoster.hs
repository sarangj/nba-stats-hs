{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module NBAStats.Endpoints.TeamRoster where

import Prelude

import NBAStats.Endpoint
import NBAStats.Response
import NBAStats.Types.Coach
import NBAStats.Types.Common
import NBAStats.Types.Player

teamRosterEndpoint :: Endpoint TeamRosterQuery TeamRosterResponse
teamRosterEndpoint = Endpoint
  { endpoint_resourceName = "commonteamroster"
  , endpoint_queryBuilder = \TeamRosterQuery{..} ->
      [ toQueryItem "TeamID" teamID
      , toQueryItem "Season" season
      ]
  , endpoint_responseHandler = \RawResponse{..} -> case resultSets of
      (rPlayers:rCoaches:_) -> TeamRosterResponse
        <$> fromJSON rPlayers
        <*> fromJSON rCoaches
      _ -> Error "Invalid Result Sets"
  }

data TeamRosterQuery = TeamRosterQuery
  { teamID :: TeamID
  , season :: Season
  } deriving (Show)

data TeamRosterResponse = TeamRosterResponse
  { players :: [Player]
  , coaches :: [Coach]
  } deriving (Show)
