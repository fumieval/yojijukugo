module Sample where

import Data.Vector.Instances ()
import System.Random.Stateful
import qualified Data.IntSet as IS
import qualified Data.Vector as V

weights :: RandomGenM g r m
  => V.Vector Int -- ^ monotonic
  -> g -> m Int
weights ws gen = do
  t <- randomRM (0, V.last ws) gen
  pure $! maybe 0 id $ V.findIndex (>=t) ws

intSet :: (RandomGenM g r m) => g -> IS.IntSet -> m (Maybe Int)
intSet gen s = case IS.splitRoot s of
  [_] -> do
    let vec = V.fromList $ IS.toList s
    i <- randomRM (0, V.length vec - 1) gen
    pure $! Just $! vec V.! i
  [l, r] -> do
    let m = IS.size l
    let n = IS.size r
    i <- randomRM (0, m + n - 1) gen
    if i < m
      then intSet gen l
      else intSet gen r
  _ -> pure Nothing