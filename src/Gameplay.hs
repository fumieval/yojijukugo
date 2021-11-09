{-# LANGUAGE TemplateHaskell #-}
module Gameplay
  ( RoomState(..)
  , wsApp
  , saveSnapshot
  , Server(..)
  , newServer
  , closeInactiveRooms
  , Config(..)
  , DictionarySource(..)
  , loadConfig
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Writer
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as C8
import Data.Function
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, UTCTime, diffUTCTime)
import Data.Vector qualified as V
import Deriving.Aeson
import Deriving.Aeson.Stock
import Logic
import Network.WebSockets qualified as WS
import Protocol
import RIO hiding ((^.), (%~), (.~), lens)
import RIO.Directory (copyFile, doesFileExist, createDirectoryIfMissing)
import RIO.FilePath ((</>))
import RIO.Text qualified as T
import UnliftIO.Concurrent (forkIO)
import qualified Prelude

defaultName :: Int -> Text
defaultName i = T.pack $ "無名　" ++ [stems V.! mod i 10, branches V.! mod i 12] where
  stems = V.fromList "甲乙丙丁戊己庚辛壬癸"
  branches = V.fromList "子丑寅卯辰巳午未申酉戌亥"

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

data RoomState = RoomState
  { players :: IM.IntMap Player
  , board :: Board
  , playerConn :: IM.IntMap WS.Connection
  , freshPlayerId :: Int
  , lastActivity :: UTCTime
  , library :: Library
  } deriving Generic
makeLenses ''RoomState

data Server = Server
  { vRooms :: TVar (IM.IntMap (TVar RoomState))
  , refLibrary :: IORef Library
  , logger :: LogFunc
  }

instance HasLogFunc Server where
  logFuncL = lens (.logger) $ \x f -> x { logger = f }

data Session = Session
  { roomId :: Int
  , vRoom :: TVar RoomState
  , conn :: WS.Connection
  , server :: Server
  }
instance HasLogFunc Session where
  logFuncL f s = logFuncL f s.server <&> \s' -> s { server = s'}

type SessionM = RIO Session

send :: ServerMessage -> SessionM ()
send msg = do
  Session{conn} <- ask
  liftIO $ WS.sendTextData conn $ J.encode msg

broadcast :: ServerMessage -> SessionM ()
broadcast msg = do
  room <- asks (.vRoom) >>= readTVarIO
  forM_ room.playerConn
    $ \conn -> liftIO (WS.sendTextData conn (J.encode msg))
    `catch` \(_ :: SomeException) -> pure ()

withPlayer :: (PlayerId -> SessionM a) -> SessionM a
withPlayer cont = bracket
  (do
    Session{..} <- ask
    now <- liftIO getCurrentTime
    result <- atomically $ do
      room <- readTVar vRoom
      let i = room.freshPlayerId
      let player = Player (defaultName i) (palette Prelude.!! mod i 12)
      writeTVar vRoom room
        { lastActivity = now
        , freshPlayerId = room.freshPlayerId + 1
        , players = room.players & at i ?~ player
        , playerConn = room.playerConn & at i ?~ conn
        }
      pure (PlayerId i, player)
    send $ PutYou result
    broadcastPlayers
    pure $ fst result)
  (\(PlayerId i) -> do
    Session{vRoom} <- ask
    atomically $ modifyTVar vRoom
      $ \r -> r
        { players = IM.delete i r.players
        , playerConn = IM.delete i r.playerConn
        }
    broadcastPlayers)
  cont

recreateBoard :: SessionM ()
recreateBoard = do
  Session{..} <- ask
  room0 <- readTVarIO vRoom
  lib <- readIORef server.refLibrary

  let nextLevel = room0.board.difficulty + 1
  board <- liftIO $ generateBoard lib roomId nextLevel (room0.board.scoreboard)
  atomically $ modifyTVar' vRoom $ \room -> room { board = board, library = lib }

  saveSnapshot roomId board
  broadcast $ PutBoard board

broadcastPlayers :: SessionM ()
broadcastPlayers = do
  room <- asks (.vRoom) >>= readTVarIO
  broadcast $ PutPlayers room.players

latestSnapshotPath :: Int -> FilePath
latestSnapshotPath roomId = "snapshots" </> show roomId </> "latest.json"

saveSnapshot :: HasLogFunc r => Int -> Board -> RIO r ()
saveSnapshot roomId board = do
  now <- liftIO getCurrentTime
  let prefix = "snapshots" </> show roomId
  createDirectoryIfMissing True prefix
  let path = prefix </> formatTime defaultTimeLocale "%Y%m%dT%H%M%S.json" now
  liftIO $ J.encodeFile path board
  logInfo $ "Saved a snapshot to " <> display (T.pack path)
  copyFile path (latestSnapshotPath roomId)

closeInactiveRooms :: RIO Server ()
closeInactiveRooms = do
  Server{..} <- ask
  m <- readTVarIO vRooms
  now <- liftIO getCurrentTime
  forM_ (IM.toList m) $ \(i, v) -> do
    room <- readTVarIO v
    when (null (room.players) && diffUTCTime now room.lastActivity > 60) $ do
      saveSnapshot i room.board
      logInfo $ "Closed room " <> display i <> " due to inactivity"
      atomically $ modifyTVar' vRooms $ IM.delete i

updateBoard :: Session -> PlayerId -> Board -> STM (SessionM ())
updateBoard Session{..} playerId board = do
  room <- readTVar vRoom
  let name = room.players ^. ix playerId.unPlayerId . RIO.to (.name)
  let (board', done) = runWriter $ checkFinish room.library board
  let board'' = board' { scoreboard = Map.insertWith (+) name (length done) board'.scoreboard }
  writeTVar vRoom $ room { board = board'' }
  pure $ do
    now <- liftIO getCurrentTime
    atomically $ modifyTVar' vRoom $ \r -> r { lastActivity = now }
    forM_ done $ broadcast . AckDone
    unless (null done) $ do
      broadcast $ PutScoreboard $ board''.scoreboard
      checkComplete

checkComplete :: SessionM ()
checkComplete = do
  Session{..} <- ask
  board <- (.board) <$> readTVarIO vRoom
  when (all (.finished) (board.jukugos)) $ do
    broadcast LevelFinished
    _ <- forkIO $ do
        threadDelay $ 3 * 1000000
        broadcast $ PutStatus ""
        recreateBoard
    broadcast $ PutStatus "次章突入"

handleMessage :: PlayerId -> ClientMessage -> SessionM ()
handleMessage playerId msg = do
  session@Session{..} <- ask
  case msg of
    Heartbeat -> pure ()
    SetPlayerName name -> do
      atomically $ modifyTVar' vRoom
        $ \room -> room { players = room.players & ix playerId.unPlayerId %~ \p -> p { name = name } }
      broadcastPlayers
    Touch i -> broadcast $ AckTouch playerId i
    Untouch i -> broadcast $ AckUntouch playerId i
    Swap (i, j) -> join $ atomically $ do
      let p = divMod i 4
      let q = divMod j 4
      room <- readTVar vRoom
      case swap p q $ room.board of
        Nothing -> pure mempty -- ERROR
        Just board -> (broadcast (AckSwap playerId i j)>>)
          <$> updateBoard session playerId board

sessionLoop :: SessionM ()
sessionLoop = withPlayer $ \playerId -> do
  Session{..} <- ask
  readTVarIO vRoom >>= \room -> send $ PutBoard (room.board)
  checkComplete

  fix $ \self -> liftIO (WS.receive conn) >>= \case
    WS.DataMessage _ _ _ (WS.Text jsonStr _) -> do
      case J.decode jsonStr of
        Nothing -> logError $ "Failed to parse: " <> displayShow jsonStr
        Just msg -> handleMessage playerId msg
      self
    WS.ControlMessage (WS.Close _ _) -> pure ()
    _ -> self

wsApp :: Server -> WS.ServerApp
wsApp server pending = do
  conn <- WS.acceptRequest pending
  _hello <- WS.receive conn
  roomId <- case filter (not . C8.null) $ C8.split '/' $ WS.requestPath $ WS.pendingRequest pending of
    ["game", str] | Just i <- readMaybe (C8.unpack str) -> pure i
    s -> error $ "Failed to parse roomId: " ++ show s
  vRoom <- acquireRoom server roomId

  runRIO Session{..} $ sessionLoop `finally` do
    room <- readTVarIO vRoom
    saveSnapshot roomId room.board

createRoom :: Server -> Int -> IO (TVar RoomState)
createRoom Server{..} roomId = do
  library <- readIORef refLibrary
  let path = latestSnapshotPath roomId
  hasSnapshot <- doesFileExist path
  now <- getCurrentTime
  board <- if hasSnapshot
    then either error id <$> J.eitherDecodeFileStrict path
    else generateBoard library roomId 2 mempty
  v <- newTVarIO $ RoomState
    { players = mempty
    , board = board
    , playerConn = mempty
    , freshPlayerId = 0
    , lastActivity = now
    , library = library
    }
  runRIO logger $ logInfo $ "Created room #" <> display roomId
  pure v

acquireRoom :: Server -> Int -> IO (TVar RoomState)
acquireRoom server@Server{..} roomId = do
  m <- readTVarIO vRooms
  case IM.lookup roomId m of
    Nothing -> do
      v <- createRoom server roomId
      atomically $ modifyTVar' vRooms $ IM.insert roomId v
      pure v
    Just room -> pure room

newServer :: Config -> LogFunc -> IO Server
newServer cfg logger = do
  vRooms <- newTVarIO mempty
  lib <- runRIO logger $ loadConfig cfg
  refLibrary <- newIORef lib
  pure Server{..}

data Config = Config
  { dictionaries :: [DictionarySource]
  , port :: Int
  } deriving (Generic)
  deriving (FromJSON, ToJSON) via Vanilla Config

data DictionarySource = DictionarySource
  { path :: FilePath
  , weight :: Int
  } deriving (Generic)
  deriving (FromJSON, ToJSON) via Vanilla DictionarySource

loadConfig :: HasLogFunc r => Config -> RIO r Library
loadConfig Config{..} = do
  dataset <- forM dictionaries $ \dic -> do
    ws <- T.lines <$> readFileUtf8 dic.path
    pure (dic.weight, ws)
  let lib = newLibrary dataset
  logInfo $ "Loaded " <> display (V.length lib.vector) <> " words"
  pure lib