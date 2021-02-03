{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import UnliftIO.Concurrent (forkIO)
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
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Data.Vector as V
import RIO hiding ((^.), (%~), (.~), lens)
import System.Environment (getArgs)
import Control.Monad.Writer
import qualified Network.Wai.Middleware.Static as W
import System.Random.Stateful
import RIO.FilePath ((</>))
import RIO.Directory (copyFile, doesFileExist, createDirectoryIfMissing)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

data ClientMessage = Touch Int
  | Untouch Int
  | Swap (Int, Int)
  | SetPlayerName Text
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

data RoomState = RoomState
  { _roomPlayers :: IM.IntMap Player
  , _roomBoard :: Board
  , _roomPlayerConn :: IM.IntMap WS.Connection
  , _roomFreshPlayerId :: Int
  , _roomRNG :: RNG
  } deriving Generic
makeLenses ''RoomState

data Server = Server
  { rooms :: TVar (IM.IntMap (TVar RoomState))
  , library :: Library
  , logger :: LogFunc
  }
instance HasLogFunc Server where
  logFuncL = lens logger $ \x f -> x { logger = f }

data Session = Session
  { sessionRoomId :: Int
  , sessionRoom :: TVar RoomState
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

defaultName :: Int -> Text
defaultName i = T.pack $ "無名　" ++ [stems V.! mod i 10, branches V.! mod i 12] where
  stems = V.fromList "甲乙丙丁戊己庚辛壬癸"
  branches = V.fromList "子丑寅卯辰巳午未申酉戌亥"

withPlayer :: (PlayerId -> SessionM a) -> SessionM a
withPlayer cont = bracket
  (do
    conn <- asks sessionConn
    vRoom <- asks sessionRoom

    result <- atomically $ do
      room <- readTVar vRoom
      let i = _roomFreshPlayerId room
      let player = Player (defaultName i) (palette Prelude.!! mod i 12)
      writeTVar vRoom
        $ roomPlayers . at i ?~ player
        $ roomPlayerConn . at i ?~ conn
        $ roomFreshPlayerId +~ 1
        $ room
      pure (PlayerId i, player)
    send $ PutYou result
    broadcastPlayers
    pure $ fst result)
  (\(PlayerId i) -> do
    vRoom <- asks sessionRoom
    atomically $ modifyTVar vRoom
      $ (roomPlayers %~ IM.delete i)
      . (roomPlayerConn %~ IM.delete i)
    broadcastPlayers)
  cont

recreateBoard :: SessionM ()
recreateBoard = do
  vRoom <- asks sessionRoom
  server <- asks sessionServer
  room0 <- readTVarIO vRoom

  let nextLevel = _boardDifficulty (_roomBoard room0) + 1.0
  board <- liftIO $ generateBoard (library server) (_roomRNG room0) nextLevel (_scoreboard $ _roomBoard room0)
  atomically $ modifyTVar vRoom $ \room -> room { _roomBoard = board }
  broadcast $ PutBoard board

broadcastPlayers :: SessionM ()
broadcastPlayers = do
  room <- asks sessionRoom >>= readTVarIO
  broadcast $ PutPlayers $ _roomPlayers room

latestSnapshotPath :: Int -> FilePath
latestSnapshotPath roomId = "snapshots" </> show roomId </> "latest.json"

saveSnapshot :: Int -> Board -> SessionM ()
saveSnapshot roomId board = do
  now <- liftIO getCurrentTime
  let prefix = "snapshots" </> show roomId
  createDirectoryIfMissing True prefix
  let path = prefix </> formatTime defaultTimeLocale "%Y%m%dT%H%M%S.json" now
  liftIO $ J.encodeFile path board
  logInfo $ "Saved a snapshot to " <> display (T.pack path)
  copyFile path (latestSnapshotPath roomId)

updateBoard :: Session -> PlayerId -> Board -> STM (SessionM ())
updateBoard Session{..} playerId board = do
  room <- readTVar sessionRoom
  let name = room ^. roomPlayers . ix (unPlayerId playerId) . playerName
  let (board', done) = runWriter $ checkFinish (library sessionServer) board
  let board'' = board' & scoreboard %~ Map.insertWith (+) name (length done)
  let room' = room
        & roomBoard .~ board''
  writeTVar sessionRoom room'
  pure $ do
    forM_ done $ broadcast . AckDone
    unless (null done) $ do
      broadcast $ PutScoreboard $ board'' ^. scoreboard
      _ <- forkIO $ saveSnapshot sessionRoomId board'
      pure ()
    when (all _finished (_jukugos board')) $ do
      broadcast LevelFinished
      _ <- forkIO $ do
          threadDelay $ 3 * 1000000
          broadcast $ PutStatus ""
          recreateBoard
      broadcast $ PutStatus "次章突入"

wsApp :: Server -> WS.ServerApp
wsApp sessionServer pending = do
  sessionConn <- WS.acceptRequest pending
  _hello <- WS.receive sessionConn
  sessionRoomId <- case filter (not . C8.null) $ C8.split '/' $ WS.requestPath $ WS.pendingRequest pending of
    ["game", str] | Just i <- readMaybe (C8.unpack str) -> pure i
    s -> error $ "Failed to parse roomId: " ++ show s
  sessionRoom <- acquireRoom sessionServer sessionRoomId
  let session = Session{..}

  runRIO session $ withPlayer $ \playerId -> do
    readTVarIO sessionRoom >>= \room -> send $ PutBoard (_roomBoard room)

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
              Untouch i -> broadcast $ AckUntouch playerId i
              Swap (i, j) -> join $ atomically $ do
                let p = divMod i 4
                let q = divMod j 4
                room <- readTVar sessionRoom
                case swap p q $ _roomBoard room of
                  Nothing -> pure mempty -- ERROR
                  Just board -> (broadcast (AckSwap playerId i j)>>)
                    <$> updateBoard session playerId board
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
      rng <- newAtomicGenM $ mkStdGen roomId
      let path = latestSnapshotPath roomId
      hasSnapshot <- doesFileExist path
      board <- if hasSnapshot
        then either error id <$> J.eitherDecodeFileStrict path
        else generateBoard library rng 2.0 mempty
      v <- newTVarIO $ RoomState
        { _roomPlayers = mempty
        , _roomBoard = board
        , _roomPlayerConn = mempty
        , _roomFreshPlayerId = 0
        , _roomRNG = rng
        }
      atomically $ modifyTVar' rooms $ IM.insert roomId v
      pure v
    Just room -> pure room

newServer :: [FilePath] -> LogFunc -> IO Server
newServer paths logger = do
  rooms <- newTVarIO mempty
  dataset <- forM paths $ \path -> T.lines . T.decodeUtf8 <$> B.readFile path
  let library = newLibrary dataset
  pure Server{..}

main :: IO ()
main = do
  paths <- getArgs
  logOptions' <- logOptionsHandle stderr True
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \lf -> do
    server <- newServer paths lf
    app <- scottyApp $ do
      get "/game/:id" $ 
        file "index.html"
    runRIO lf $ logInfo "Started the server"
    Warp.run 4444
      $ WS.websocketsOr WS.defaultConnectionOptions (wsApp server)
      $ W.static app
