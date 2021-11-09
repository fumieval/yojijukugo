{-# LANGUAGE TemplateHaskell #-}
module Logic
  ( Library(..)
  , Character(..)
  , Jukugo(..)
  , finished
  , Scoreboard
  , Board(..)
  , scoreboard
  , boardDifficulty
  , Position
  , swap
  , generateBoard
  , checkFinish
  , newLibrary
  , prop_no_stuck) where

import Control.Lens hiding (Prefixed)
import Control.Lens.Unsound
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.HashSet qualified as HS
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Instances ()
import Deriving.Aeson.Stock
import RIO hiding ((^.), (%~), (.~), lens, (^?))
import Sample qualified
import System.Random.Stateful
import Test.QuickCheck qualified as QC

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

type Scoreboard = Map.Map T.Text Int

data Board = Board
  { _jukugos :: IM.IntMap Jukugo
  , _boardDifficulty :: Int
  , _scoreboard :: Scoreboard
  } deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Prefixed "_" Board
makeLenses ''Board

type Position = (Int, Int)

swap :: Position -> Position -> Board -> Maybe Board
swap (r0, c0) (r1, c1)
  | r0 == r1 = jukugos . ix r0 $ \jukugo -> do
      guard $ not $ _finished jukugo
      a <- jukugo ^? content . ix c0
      b <- jukugo ^? content . ix c1
      pure
          $ content . ix c0 .~ b
          $ content . ix c1 .~ a
          $ jukugo
  | otherwise = jukugos . lensProduct (at r0) (at r1) $ \case
    (Just alpha, Just bravo) -> do
      guard $ not $ _finished alpha
      guard $ not $ _finished bravo
      a <- alpha ^? content . ix c0
      b <- bravo ^? content . ix c1
      let alpha' = alpha & content . ix c0 .~ b
      let bravo' = bravo & content . ix c1 .~ a
      pure (Just alpha', Just bravo')
    x -> pure x

randomise :: StdGen -> Board -> Board
randomise gen = jukugos %~ Sample.shuffle gen (traverse . content . each)

generateBoard
  :: Library -- ^ all list of jukugos
  -> Int
  -> Int
  -> Scoreboard
  -> IO Board
generateBoard library seed difficulty scores = do
  let population = floor (1.5 ** fromIntegral difficulty :: Double)
  let (boardGen, shuffleGen) = split $ mkStdGen $ seed * 256 + difficulty
  let jukugos' = tabulate $ populate library boardGen population
  pure $! randomise shuffleGen
    (Board jukugos' difficulty scores)

type CharSet = IS.IntSet

toCharSet :: V.Vector Char -> IS.IntSet
toCharSet = IS.fromList . map fromEnum . V.toList

tabulate :: IM.IntMap (V.Vector Char) -> IM.IntMap Jukugo
tabulate m = IM.fromList
  [ (i, Jukugo False $ V.zipWith Character (V.enumFromTo (i * 4) (i * 4 + 3)) v)
  | (i, v) <- zip [0..] $ IM.elems m
  ]

-- | 難易度で重みづけ、四字熟語のインデックスを返す
sampleLibrary :: Library -> IS.IntSet -> State StdGen (Maybe Int)
sampleLibrary Library{..} available = do
  difficulty <- Sample.weights (V.zipWith const difficultyWeights $ V.tail libraryDifficulty) StateGenM
  let filterL = snd $ IS.split (libraryDifficulty V.! difficulty - 1) available
  let filterR = fst $ IS.split (libraryDifficulty V.! succ difficulty + 1) filterL
  Sample.intSet StateGenM filterR

populate
  :: Library
  -> StdGen
  -> Int
  -> IM.IntMap (V.Vector Char)
populate lib@Library{..} gen0 population = flip evalState gen0
  $ go (IS.fromList [0..V.length libraryV - 1]) IS.empty 0 IM.empty (population * 8) where
  go :: IS.IntSet
    -> CharSet
    -> Int
    -> IM.IntMap (V.Vector Char)
    -> Int
    -> State StdGen (IM.IntMap (V.Vector Char))
  go _ _ _ board 0 = pure board
  go available _ _ board _ | IS.null available = pure board
  go _ _ boardSize board _ | boardSize >= population = pure board
  go !available !pool !boardSize !board !counter = do

    let trial = do
          -- pick a candidate
          i <- MaybeT $ sampleLibrary lib available

          let reachable s = all ((`IS.member` s) . fromEnum)
          let jukugo = libraryV V.! i
          let cset = toCharSet jukugo

          -- reject if it can be created from the existing set
          guard $ not $ reachable pool jukugo

          let pool' = pool `IS.union` cset

          let ambiguity = [T.pack $ V.toList other | j <- [0..V.length libraryV - 1]
                , IM.notMember j board
                , let other = libraryV V.! j
                , toCharSet other /= cset -- 色即是空　空即是色
                , reachable pool' other]
          guard $ case ambiguity of
            _ : _ : _ -> False
            _ -> True
          return $ go
            (IS.delete i available)
            pool'
            (boardSize + 1)
            (IM.insert i jukugo board)
            (counter - 1)

    runMaybeT trial >>= \case
      Nothing -> go available pool boardSize board (counter - 1)
      Just cont -> cont

checkFinish :: Library
  -> Board
  -> Writer [Int] Board
checkFinish library board = do
  jukugos' <- iforM (_jukugos board)
    $ \i jukugo -> if not (_finished jukugo) && HS.member (fmap charC $ _content jukugo) (libraryS library)
      then do
        tell [i]
        pure jukugo { _finished = True }
      else pure jukugo
  pure board { _jukugos = jukugos' }

data Library = Library
  { libraryS :: HS.HashSet (V.Vector Char)
  , libraryV :: V.Vector (V.Vector Char)
  , libraryDifficulty :: V.Vector Int
  , difficultyWeights :: V.Vector Int
  }

newLibrary :: [(Int, [T.Text])] -> Library
newLibrary dataset = Library{..} where
  vecs :: [V.Vector (V.Vector Char)]
  vecs = V.fromList . map (V.fromList . T.unpack) . snd <$> dataset
  libraryS = HS.fromList $ V.toList libraryV
  libraryV = V.concat vecs
  libraryDifficulty = V.fromList $ scanl (+) 0 $ map V.length vecs
  difficultyWeights = V.postscanl' (+) 0 $ V.fromList $ map fst dataset

prop_no_stuck :: Int -> QC.Property
prop_no_stuck seed = HS.member (V.fromList target) jukugoSet QC.=== HS.isSubsetOf (HS.fromList target) charSet where
  target = "変幻自在"
  jukugoSet = HS.fromList $ IM.elems $ populate lib (mkStdGen seed) 6
  charSet = HS.fromList $ foldMap V.toList $ HS.toList jukugoSet
  lib = newLibrary [(,) 1 $ fmap T.pack $ words "幻影旅団 心神耗弱 異口同音 変態百出 三位一体 打成一片 変幻自在 一心同体"]