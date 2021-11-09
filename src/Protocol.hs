module Protocol where

import Data.IntMap.Strict qualified as IM
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
  { name :: Text
  , color :: String
  } deriving Generic
  deriving (FromJSON, ToJSON) via Vanilla Player

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
