{-# LANGUAGE TemplateHaskell #-}
module Logic where

import RIO hiding ((^.), (%~), (.~), lens, (^?))
import Control.Lens
import Control.Lens.Unsound
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Data.Vector.Instances ()
import Deriving.Aeson.Stock
import System.Random.Stateful
import Control.Monad.Trans.State
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

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
  , _boardDifficulty :: Double
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

type RNG = AtomicGenM StdGen

randomise :: RNG -> Board -> IO Board
randomise gen board = do
  i <- randomRM (0, IM.size (_jukugos board) - 1) gen
  j <- randomRM (0, IM.size (_jukugos board) - 1) gen
  p <- randomRM (0, 3) gen
  q <- randomRM (0, 3) gen
  maybe (error "Failed to swap!") pure $ swap (i, p) (j, q) board

generateBoard
  :: Library -- ^ all list of jukugos
  -> RNG
  -> Double
  -> Scoreboard
  -> IO Board
generateBoard library gen difficulty scores = do
  let population = floor $ 1.5 ** difficulty
  seed <- randomM gen
  let jukugos' = tabulate $ populate library seed population
  foldM (\b _ -> randomise gen b) (Board jukugos' difficulty scores) [0..population * population * 4]

type CharSet = IS.IntSet

toCharSet :: V.Vector Char -> IS.IntSet
toCharSet = IS.fromList . map fromEnum . V.toList

sampleIntSet :: (RandomGenM g r m) => g -> IS.IntSet -> m (Maybe Int)
sampleIntSet gen s = case IS.splitRoot s of
  [_] -> do
    let vec = V.fromList $ IS.toList s
    i <- randomRM (0, V.length vec - 1) gen
    pure $! Just $! vec V.! i
  [l, r] -> do
    let m = IS.size l
    let n = IS.size r
    i <- randomRM (0, m + n - 1) gen
    if i < m
      then sampleIntSet gen l
      else sampleIntSet gen r
  _ -> pure Nothing

difficultyWeights :: V.Vector Int
difficultyWeights = V.postscanl' (+) 0 $ V.fromList [8,3,4,1]

sampleDifficulty :: RandomGenM g r m => V.Vector Int -> g -> m Int
sampleDifficulty weights gen = do
  t <- randomRM (0, V.last weights) gen
  pure $! maybe 0 id $ V.findIndex (>=t) weights

tabulate :: IM.IntMap (V.Vector Char) -> IM.IntMap Jukugo
tabulate m = IM.fromList
  [ (i, Jukugo False $ V.zipWith Character (V.enumFromTo (i * 4) (i * 4 + 3)) v)
  | (i, v) <- zip [0..] $ IM.elems m
  ]

populate
  :: Library
  -> Int
  -> Int
  -> IM.IntMap (V.Vector Char)
populate Library{..} seed population = flip evalState (mkStdGen seed)
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
    difficulty <- sampleDifficulty (V.zipWith const difficultyWeights $ V.tail libraryDifficulty) StateGenM
    let filterL = snd $ IS.split (libraryDifficulty V.! difficulty - 1) available
    let filterR = fst $ IS.split (libraryDifficulty V.! succ difficulty + 1) filterL

    let trial = do
          -- pick a candidate
          i <- MaybeT $ sampleIntSet StateGenM filterR

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
  }

newLibrary :: [[T.Text]] -> Library
newLibrary dataset = Library{..} where
  vecs :: [V.Vector (V.Vector Char)]
  vecs = V.fromList . map (V.fromList . T.unpack) <$> dataset
  libraryS = HS.fromList $ V.toList libraryV
  libraryV = V.concat vecs
  libraryDifficulty = V.fromList $ scanl (+) 0 $ map V.length vecs

newLibraryFromFiles :: [FilePath] -> IO Library
newLibraryFromFiles paths = do
  dataset <- forM paths $ \path -> T.lines <$> readFileUtf8 path
  pure $! newLibrary dataset

prop_no_stuck :: Int -> QC.Property
prop_no_stuck seed = HS.member (V.fromList target) jukugoSet QC.=== HS.isSubsetOf (HS.fromList target) charSet where
  target = "変幻自在"
  jukugoSet = HS.fromList $ IM.elems $ populate lib seed 6
  charSet = HS.fromList $ foldMap V.toList $ HS.toList jukugoSet
  lib = newLibrary [fmap T.pack $ words "幻影旅団 心神耗弱 異口同音 変態百出 三位一体 打成一片 変幻自在 一心同体"]