{-# LANGUAGE TemplateHaskell #-}
module Logic where

import Control.Lens
import Control.Lens.Unsound
import Control.Monad.Trans.Writer
import Control.Monad
import Data.Vector.Instances ()
import Deriving.Aeson.Stock
import System.Random.Stateful
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Character = Character
  { charI :: Int
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

type Library = V.Vector (V.Vector Char)

generateBoard
  :: Library -- ^ all list of jukugos
  -> RNG -> Int -- ^ number of jukugos
  -> IO Board
generateBoard library gen population = do
  jukugos' <- populate library gen population
  foldM (\b _ -> randomise gen b) (Board jukugos') [0..population * population * 4]

type CharSet = IS.IntSet

toCharSet :: V.Vector Char -> IS.IntSet
toCharSet = IS.fromList . map fromEnum . V.toList

sampleIntSet :: (RandomGenM g r m) => g -> IS.IntSet -> m Int
sampleIntSet gen s = case IS.splitRoot s of
  [_] -> do
    let vec = V.fromList $ IS.toList s
    i <- randomRM (0, V.length vec - 1) gen
    pure $! vec V.! i
  [l, r] -> do
    let m = IS.size l
    let n = IS.size r
    i <- randomRM (0, m + n - 1) gen
    if i < m
      then sampleIntSet gen l
      else sampleIntSet gen r
  _ -> error "FIXME: splitRoot"

populate
  :: Library
  -> RNG
  -> Int
  -> IO (IM.IntMap Jukugo)
populate library gen count = go (IS.fromList [0..V.length library - 1]) IS.empty IM.empty count where
  go _ _ board (-1) = pure board
  go available _ board _ | IS.null available = pure board
  go available pool board counter = do
    i <- sampleIntSet gen available
    let jukugo = library V.! i
    let cset = toCharSet jukugo
    let !pool' = pool `IS.union` cset
    let !available' = IS.filter (\j -> not $ all ((`IS.member` pool') . fromEnum) (library V.! j)) available
    let !jukugo' = Jukugo False $ V.zipWith Character (V.enumFromTo (counter * 4) (counter * 4 + 3)) jukugo
    go available' pool' (IM.insert counter jukugo' board) (counter - 1)

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