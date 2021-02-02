{-# LANGUAGE TemplateHaskell #-}
module Logic where

import Control.Lens
import Control.Lens.Unsound
import Control.Monad.Trans.Writer
import Control.Monad
import Data.Text (Text)
import Data.Traversable
import Data.Vector.Instances ()
import Deriving.Aeson.Stock
import System.Random.Stateful
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Vector as V

data Character = Character
  { charI :: Text
  , charC :: Char
  } deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via PrefixedSnake "char" Character

data Jukugo = Jukugo
  { _finished :: Bool
  , _content :: V.Vector Character
  } deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Prefixed "_" Jukugo
makeLenses ''Jukugo

data Board = Board
  { _jukugos :: IM.IntMap Jukugo
  } deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Prefixed "_" Board
makeLenses ''Board

type Position = (Int, Int)

swap :: Position -> Position -> Board -> Maybe Board
swap (r0, c0) (r1, c1)
  | r0 == r1 = jukugos . ix r0 $ \jukugo -> do
      a <- jukugo ^? content . ix c0
      b <- jukugo ^? content . ix c1
      pure
          $ content . ix c0 .~ b
          $ content . ix c1 .~ a
          $ jukugo
  | otherwise = jukugos . lensProduct (at r0) (at r1) $ \case
    (Just alpha, Just bravo) -> do
      a <- alpha ^? content . ix c0
      b <- bravo ^? content . ix c1
      let alpha' = alpha & content . ix c0 .~ b
      let bravo' = bravo & content . ix c1 .~ a
      pure (Just alpha', Just bravo')
    x -> pure x

type RNG = AtomicGenM StdGen

randomise :: RNG -> Board -> IO Board
randomise gen board = do
  i <- randomRM (0, IM.size (_jukugos board) - 1) gen
  j <- randomRM (0, IM.size (_jukugos board) - 1) gen
  p <- randomRM (0, 3) gen
  q <- randomRM (0, 3) gen
  maybe (error "Failed to swap!") pure $ swap (i, p) (j, q) board

generateBoard
  :: V.Vector (V.Vector Char) -- ^ all list of jukugos
  -> RNG -> Int -- ^ number of jukugos
  -> IO Board
generateBoard library gen population = do
  jukugos' <- fmap IM.fromList $ for [0..population - 1] $ \i -> do
    j <- randomRM (0, V.length library - 1) gen -- TODO: prevent duplicates
    let jukugo = library V.! j
    uid <- V.replicateM 4 (T.pack . (show :: Word -> String) <$> randomM gen)
    pure (i, Jukugo False $ V.zipWith Character uid jukugo)
  foldM (\b _ -> randomise gen b) (Board jukugos') [0..population * population * 4]

checkFinish :: HS.HashSet (V.Vector Char)
  -> Board
  -> Writer [Int] Board
checkFinish library board = do
  jukugos' <- iforM (_jukugos board)
    $ \i jukugo -> if not (_finished jukugo) && HS.member (fmap charC $ _content jukugo) library
      then do
        tell [i]
        pure jukugo { _finished = True }
      else pure jukugo
  pure board { _jukugos = jukugos' }