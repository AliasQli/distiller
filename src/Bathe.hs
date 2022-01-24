{-# LANGUAGE OverloadedLists #-}

module Bathe where

import           Data.List           (unfoldr)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           System.Random

import           Control.Monad.State
import           Data.Bifunctor
import           Data.Functor
import           Type

rand :: (Int, Int) -> State StdGen Int
rand (a, b) = do
  gen <- get
  let (r, gen') = uniformR (a, b) gen
  put gen'
  pure r

choose :: Vector (a, Int) -> State StdGen a
choose vec = do
  i <- rand (1, snd (vec V.! 0))
  let vector = V.zipWith (\(t, a) (_, b) -> (t, (a, b))) vec (V.tail vec `V.snoc` (undefined, 0))
      search' vec
        | i > a     = search' (V.take k     vec)
        | i <= b    = search' (V.drop (k+1) vec)
        | otherwise = t
        where
          k = V.length vec `div` 2
          (t, (a, b)) = vec V.! k
  pure $ search' vector

predict :: Matrices -> Index -> State StdGen (Maybe (Token, Index))
predict ((m11, m12, m13, m14), (m21, m22, m23), (m31, m32)) (k1, k2, k3, k4) =
    choose (V.zip vs (V.scanr1 (+) ns)) >>= choose <&> \case End -> Nothing; t -> Just (t, (k2, k3, k4, t))
    where
      weights = [8, 12, 18, 27]
      (vs, ws) = V.unzip $ V.catMaybes $ V.zipWith (\i -> fmap (,i))
        (V.concat
          [weights
          , V.zipWith (*) weights (V.tail weights)
          , V.zipWith3 (\a b c -> a * b * c) weights (V.tail weights) (V.drop 2 weights)
          ])
        (V.concat
          [ V.zipWith (M.!?) [m11, m12, m13, m14] [k1, k2, k3, k4]
          , V.zipWith (M.!?) [m21, m22, m23] [(k1, k2), (k2, k3), (k3, k4)]
          , V.zipWith (M.!?) [m31, m32] [(k1, k2, k3), (k2, k3, k4)]
          ])
      ns = V.zipWith (*) ws $ fmap (snd . (V.! 0)) vs

bathe :: Matrices -> StdGen -> Text
bathe m gen = T.unwords $ unMid <$> unfoldr (\(i, g) -> let (may, g') = runState (predict m i) g in second (,g') <$> may) ((Beg, Beg, Beg, Beg), gen)
