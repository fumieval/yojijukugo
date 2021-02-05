{-# LANGUAGE TemplateHaskell #-}
module Protocol where

import Control.Lens (makeLenses)
import qualified Data.IntMap.Strict as IM
import Deriving.Aeson
import Deriving.Aeson.Stock
import Data.Text (Text)
import Logic

data ClientMessage = Touch Int
  | Untouch Int
  | Swap (Int, Int)
  | SetPlayerName Text
  | Heartbeat
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[TagSingleConstructors, SumObjectWithSingleField] ClientMessage

data Player = Player
  { _playerName :: Text
  , _playerColor :: String
  } deriving Generic
  deriving (FromJSON, ToJSON) via PrefixedSnake "_player" Player
makeLenses ''Player

data ServerMessage = PutBoard Board
  | PutScoreboard Scoreboard
  | PutPlayers (IM.IntMap Player)
  | PutYou (PlayerId, Player)
  | AckTouch PlayerId Int
  | AckUntouch PlayerId Int
  | AckSwap PlayerId Int Int
  | AckDone Int
  | PutStatus Text
  | LevelFinished
  deriving Generic
  deriving (FromJSON, ToJSON) via CustomJSON '[TagSingleConstructors, SumObjectWithSingleField] ServerMessage

newtype PlayerId = PlayerId { unPlayerId :: Int } deriving (FromJSON, ToJSON, Show)
