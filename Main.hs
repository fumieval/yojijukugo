{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Prelude
import Control.Lens
import Control.Monad
import Data.Function
import Deriving.Aeson
import Deriving.Aeson.Stock
import Logic
import Web.Scotty
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import RIO hiding ((%~), (.~), lens)
import System.Environment (getArgs)
import Control.Monad.Writer

data ClientMessage = Touch Int
  | Swap (Int, Int)
  | SetPlayerName String
  | Heartbeat
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[TagSingleConstructors, SumObjectWithSingleField] ClientMessage

newtype PlayerId = PlayerId { unPlayerId :: Int } deriving (FromJSON, ToJSON, Show)

palette :: [String]
palette =
  [ "#00DD50"
  , "#FF00FF"   
  , "#DDDD00"
  , "#40FF40"
  , "#8000FF"
  , "#0080FF"
  , "#FF0000"
  , "#00DDDD"
  , "#FF8000"
  , "#0000EF"
  , "#40A000"
  , "#FF0080"
  ]


data Player = Player
  { _playerName :: String
  , _playerColor :: String
  } deriving Generic
  deriving (FromJSON, ToJSON) via PrefixedSnake "_player" Player
makeLenses ''Player

data ServerMessage = PutBoard Board
  | PutPlayers (IM.IntMap Player)
  | PutYou (PlayerId, Player)
  | AckTouch PlayerId Int
  | AckSwap PlayerId Int Int
  | AckDone Int
  deriving Generic
  deriving (FromJSON, ToJSON) via CustomJSON '[TagSingleConstructors, SumObjectWithSingleField] ServerMessage

data RoomState = RoomState
  { _roomPlayers :: IM.IntMap Player
  , _roomBoard :: Board
  , _roomPlayerConn :: IM.IntMap WS.Connection
  , _roomFreshPlayerId :: Int
  } deriving Generic
makeLenses ''RoomState

data Server = Server
  { rooms :: TVar (IM.IntMap (TVar RoomState))
  , libraryS :: HS.HashSet (V.Vector Char)
  , libraryV :: V.Vector (V.Vector Char)
  , logger :: LogFunc
  }
instance HasLogFunc Server where
  logFuncL = lens logger $ \x f -> x { logger = f }

data Session = Session
  { sessionRoom :: TVar RoomState
  , sessionConn :: WS.Connection
  , sessionServer :: Server
  }
instance HasLogFunc Session where
  logFuncL f s = logFuncL f (sessionServer s) <&> \s' -> s { sessionServer = s'}

type SessionM = RIO Session

send :: ServerMessage -> SessionM ()
send msg = do
  conn <- asks sessionConn
  liftIO $ WS.sendTextData conn $ J.encode msg

broadcast :: ServerMessage -> SessionM ()
broadcast msg = do
  room <- asks sessionRoom >>= readTVarIO
  forM_ (_roomPlayerConn room)
    $ \conn -> liftIO (WS.sendTextData conn (J.encode msg))
    `catch` \(_ :: SomeException) -> pure ()

withPlayer :: (PlayerId -> Player -> SessionM a) -> SessionM a
withPlayer cont = bracket
  (do
    conn <- asks sessionConn
    vRoom <- asks sessionRoom

    atomically $ do
      room <- readTVar vRoom
      let i = _roomFreshPlayerId room
      let player = Player (show i) (palette Prelude.!! mod i 12)
      writeTVar vRoom
        $ roomPlayers . at i ?~ player
        $ roomPlayerConn . at i ?~ conn
        $ roomFreshPlayerId +~ 1
        $ room
      pure (PlayerId i, player))
  (\(PlayerId i, _) -> do
    vRoom <- asks sessionRoom
    atomically $ modifyTVar vRoom
      $ (roomPlayers %~ IM.delete i)
      . (roomPlayerConn %~ IM.delete i)
      )
  (uncurry cont)

wsApp :: Server -> WS.ServerApp
wsApp sessionServer pending = do
  sessionConn <- WS.acceptRequest pending
  _hello <- WS.receive sessionConn
  roomId <- case filter (not . C8.null) $ C8.split '/' $ WS.requestPath $ WS.pendingRequest pending of
    ["game", str] | Just i <- readMaybe (C8.unpack str) -> pure i
    s -> error $ "Failed to parse roomId: " ++ show s
  sessionRoom <- acquireRoom sessionServer roomId

  runRIO Session{..} $ withPlayer $ \playerId player -> do
    send $ PutYou (playerId, player)

    let broadcastPlayers = do
          room <- readTVarIO sessionRoom
          broadcast $ PutPlayers $ _roomPlayers room

    readTVarIO sessionRoom >>= send . PutBoard . _roomBoard
    broadcastPlayers

    fix $ \self -> do
      liftIO (WS.receive sessionConn) >>= \case
        WS.DataMessage _ _ _ (WS.Text jsonStr _) -> do
          case J.decode jsonStr of
            Nothing -> logError $ "Failed to parse: " <> displayShow jsonStr
            Just msg -> case msg of
              Heartbeat -> pure ()
              SetPlayerName name -> do
                atomically $ do
                  modifyTVar' sessionRoom $ roomPlayers . ix (unPlayerId playerId) . playerName .~ name
                broadcastPlayers
              Touch i -> broadcast $ AckTouch playerId i
              Swap (i, j) -> do
                join $ atomically $ do
                  let p = divMod i 4
                  let q = divMod j 4
                  room <- readTVar sessionRoom
                  case swap p q $ _roomBoard room of
                    Nothing -> pure mempty
                    Just board -> do
                      let (board', done) = runWriter $ checkFinish (libraryS sessionServer) board
                      writeTVar sessionRoom $ room { _roomBoard = board' }
                      pure $ do
                        broadcast $ AckSwap playerId i j
                        forM_ done $ broadcast . AckDone
          self
        WS.ControlMessage (WS.Close _ _) -> pure ()
        _ -> self
      pure ()

acquireRoom :: Server -> Int -> IO (TVar RoomState)
acquireRoom Server{..} roomId = do
  m <- readTVarIO rooms
  case IM.lookup roomId m of
    Nothing -> do
      -- create a room
      board <- generateBoard libraryV 8
      v <- newTVarIO $ RoomState mempty board mempty 0
      atomically $ modifyTVar' rooms $ IM.insert roomId v
      pure v
    Just room -> pure room

newServer :: FilePath -> LogFunc -> IO Server
newServer path logger = do
  rooms <- newTVarIO mempty
  libraryV <- V.fromList . map (V.fromList . T.unpack) . T.lines . T.decodeUtf8 <$> B.readFile path
  let libraryS = HS.fromList $ V.toList libraryV
  pure Server{..}

main :: IO ()
main = do
  path : _ <- getArgs
  logOptions' <- logOptionsHandle stderr True
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \lf -> do
    server <- newServer path lf
    app <- scottyApp $ do
      get "/game/:id" $ 
        file "index.html"
    runRIO lf $ logInfo "Started the server"
    Warp.run 3000
      $ WS.websocketsOr WS.defaultConnectionOptions (wsApp server) app
