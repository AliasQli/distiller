module Distill where

import           Control.Monad
import           Data.Bifunctor
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

import           Type

{-# INLINE replacing #-}
replacing :: [(Text, Text)]
replacing = [("。", " 。 ")] <> concatMap @[]
  (\(c, e) -> [(c, " " <> c <> " "), (e, " " <> c <> " ")])
  [("，", ","), ("？", "?"), ("！", "!"), ("（", "("), ("）",")")]

splitWords :: Text -> Vector Token
splitWords t =
  V.fromList [Beg, Beg, Beg, Beg] <>
  V.fromList (fmap Mid (filter (/= "") $ T.splitOn " " $ foldr (uncurry T.replace) t replacing)) <>
  V.fromList [End]

pair :: Vector Token -> Vector (Index, Token)
pair xs = V.zip (V.zip4 xs (V.tail xs) (V.drop 2 xs) (V.drop 3 xs)) (V.drop 4 xs)

addPair :: (Index, Token) -> Matrices -> Matrices
addPair ((k1, k2, k3, k4), t) ((m11, m12, m13, m14), (m21, m22, m23), (m31, m32)) =
  ( (add k1 m11, add k2 m12, add k3 m13, add k4 m14)
  , (add (k1, k2) m21, add (k2, k3) m22, add (k3, k4) m23)
  , (add (k1, k2, k3) m31, add (k2, k3, k4) m32)
  )
  where
    add :: Ord k => k -> Map k (Vector (Token, Int)) -> Map k (Vector (Token, Int))
    add k m = case m M.!? k of
      Nothing -> M.insert k (V.singleton (t, 1)) m
      Just vec -> M.update (const (Just res)) k m
        where
          res = case V.findIndex ((== t) . fst) vec of
            Nothing -> (t, snd (vec V.! 0) + 1) `V.cons` vec
            Just i  -> vec |> V.modify $ \v ->
              forM_ @[] [0..i] $ \i -> MV.modify v (second (+1)) i

initMat :: Matrices
initMat =
  ( (M.empty, M.empty, M.empty, M.empty)
  , (M.empty, M.empty, M.empty)
  , (M.empty, M.empty)
  )

distill :: Text -> Matrices
distill = foldr (\t m -> foldr addPair m (pair (splitWords t))) initMat . filter (/= "") . T.lines
