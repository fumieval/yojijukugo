{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO hiding ((^.), (%~), (.~), lens)

import Control.Lens
import Data.Aeson qualified as J
import Data.Yaml qualified as Yaml
import Gameplay
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WS
import Network.Wai.Middleware.Static qualified as W
import Network.WebSockets qualified as WS
import Protocol
import RIO.Text qualified as T
import System.Environment (getArgs)
import UnliftIO.Concurrent (forkIO)
import Web.Scotty
import qualified Prelude

main :: IO ()
main = do
  [cfgPath] <- getArgs
  logOptions' <- logOptionsHandle stderr True
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \lf -> do
    cfg <- Yaml.decodeFileThrow cfgPath
    server <- newServer cfg lf
    app <- scottyApp $ do
      get "/game/:id" $
        file "index.html"

    _ <- forkIO $ forever $ do
      threadDelay $ 10 * 1000000
      runRIO server closeInactiveRooms
    _ <- forkIO $ Warp.run cfg.port
      $ WS.websocketsOr WS.defaultConnectionOptions (wsApp server)
      $ W.static app
    runRIO server $ do
      logInfo "Started the server"
      forever (liftIO prompt >>= commandHandler) `finally` shutdown

shutdown :: RIO Server ()
shutdown = do
  server <- ask
  rooms <- readTVarIO $ server.vRooms
  iforM_ rooms $ \i vRoom -> do
    room <- readTVarIO vRoom
    saveSnapshot i room.board

commandHandler :: [String] -> RIO Server ()
commandHandler = \case
  "message" : msg -> do
    server <- ask
    rooms <- readTVarIO server.vRooms
    forM_ rooms $ \vRoom -> readTVarIO vRoom >>= \room -> forM_ room.playerConn
      $ \conn -> liftIO $ WS.sendTextData conn (J.encode $ PutStatus $ T.pack $ Prelude.unwords msg)
        `catch` \(_ :: SomeException) -> pure ()
  "reload" : path : _ -> do
    server <- ask
    cfg <- Yaml.decodeFileThrow path
    loadConfig cfg >>= writeIORef server.refLibrary
  _ -> liftIO (Prelude.putStrLn "?")

prompt :: IO [String]
prompt = do
  Prelude.putStr "> " >> hFlush stdout
  Prelude.words <$> Prelude.getLine
