module Sample where

import Control.Lens (ATraversal', cloneTraversal)
import Control.Monad.Trans.State
import Data.Functor.Compose
import Data.Heap qualified as H
import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Data.Vector.Instances ()
import System.Random.Stateful

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

shuffle :: forall s a gen. RandomGen gen => gen -> ATraversal' s a -> s -> s
shuffle gen0 trav struct =
  let (popper, (h, _)) = runState
        (getCompose $ cloneTraversal trav act struct)
        (H.empty, gen0)
  in evalState popper h
  where
    act :: a -> Compose (State (H.Heap (H.Entry Int a), gen)) (State (H.Heap (H.Entry Int a))) a
    act a = Compose $ state $ \(h, gen) ->
      let (r, gen') = uniform gen
          !h' = H.insert (H.Entry r a) h
      in (pop, (h', gen'))
    pop = state $ \h -> case H.viewMin h of
      Nothing -> error "Impossible"
      Just (H.Entry _ a, h') -> (a, h')
