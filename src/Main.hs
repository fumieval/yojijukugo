{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO hiding ((^.), (%~), (.~), lens)

import UnliftIO.Concurrent (forkIO)
import qualified Prelude
import Control.Lens
import Protocol
import Gameplay
import Web.Scotty
import qualified Data.Aeson as J
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import System.Environment (getArgs)
import qualified Network.Wai.Middleware.Static as W
import qualified RIO.Text as T
import qualified Data.Yaml as Yaml

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
    _ <- forkIO $ Warp.run (cfgPort cfg)
      $ WS.websocketsOr WS.defaultConnectionOptions (wsApp server)
      $ W.static app
    runRIO server $ do
      logInfo "Started the server"
      forever (liftIO prompt >>= commandHandler) `finally` do
        rooms <- readTVarIO $ rooms server
        iforM_ rooms $ \i vRoom -> do
          board <- _roomBoard <$> readTVarIO vRoom
          saveSnapshot i board

commandHandler :: [String] -> RIO Server ()
commandHandler = \case
  "message" : msg -> do
    server <- ask
    rooms <- readTVarIO $ rooms server
    forM_ rooms $ \vRoom -> readTVarIO vRoom >>= \room -> forM_ (_roomPlayerConn room)
      $ \conn -> liftIO $ WS.sendTextData conn (J.encode $ PutStatus $ T.pack $ Prelude.unwords msg)
        `catch` \(_ :: SomeException) -> pure ()
  "reload" : path : _ -> do
    server <- ask
    cfg <- Yaml.decodeFileThrow path
    loadConfig cfg >>= writeIORef (refLibrary server)
  _ -> liftIO (Prelude.putStrLn "?")

prompt :: IO [String]
prompt = do
  Prelude.putStr "> " >> hFlush stdout
  Prelude.words <$> Prelude.getLine
