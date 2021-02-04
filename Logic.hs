{-# LANGUAGE TemplateHaskell #-}
module Logic where

import RIO hiding ((^.), (%~), (.~), lens, (^?))
import Control.Lens
import Control.Lens.Unsound
import Control.Monad.Trans.Writer
import Data.Vector.Instances ()
import Deriving.Aeson.Stock
import System.Random.Stateful
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Text as T

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
  jukugos' <- tabulate <$> populate library gen population
  foldM (\b _ -> randomise gen b) (Board jukugos' difficulty scores) [0..population * population * 4]

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
  -> RNG
  -> Int
  -> IO (IM.IntMap (V.Vector Char))
populate Library{..} gen population = go (IS.fromList [0..V.length libraryV - 1]) IS.empty 0 IM.empty (population * 8) where
  go :: IS.IntSet
    -> CharSet
    -> Int
    -> IM.IntMap (V.Vector Char)
    -> Int
    -> IO (IM.IntMap (V.Vector Char))
  go _ _ _ board 0 = pure board
  go available _ _ board _ | IS.null available = pure board
  go _ _ boardSize board _ | boardSize >= population = pure board
  go available pool boardSize board counter = do
    difficulty <- sampleDifficulty (V.zipWith const difficultyWeights $ V.tail libraryDifficulty) gen
    let filterL = snd $ IS.split (libraryDifficulty V.! difficulty) available
    let filterR = fst $ IS.split (libraryDifficulty V.! succ difficulty) filterL
    if IS.null filterR
      then go available pool boardSize board (counter - 1)
      else do
        i <- sampleIntSet gen filterR
        let jukugo = libraryV V.! i
        let cset = toCharSet jukugo
        let !nextPool = pool `IS.union` cset
        let valid s = not . all ((`IS.member` s) . fromEnum)
        let !available' = IS.filter (valid nextPool . (libraryV V.!)) available
        let history = IM.filterWithKey (\j v -> valid (pool `IS.difference` toCharSet (libraryV V.! j)) v) board
        let newSize = IM.size history + 1
        if newSize * 2 <= boardSize -- reject a candidate which shrinks the solution
          then go available pool boardSize board (counter - 1)
          else go available' nextPool newSize (IM.insert i jukugo history) counter

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