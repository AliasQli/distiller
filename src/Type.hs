{-# LANGUAGE DeriveAnyClass  #-}

module Type where

import           Codec.Serialise
import           Data.Function
import           Data.Map            (Map)
import           Data.Text           (Text)
import           Data.Vector         (Vector)
import           GHC.Generics

(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixr 0 |>

data Token = Beg | Mid {unMid :: Text} | End deriving (Show, Eq, Ord, Generic, Serialise)

type Index = (Token, Token, Token, Token)

type Index1 = Token
type Index2 = (Token, Token)
type Index3 = (Token, Token, Token)

type Matrix1 = Map Index1 (Vector (Token, Int))
type Matrix2 = Map Index2 (Vector (Token, Int))
type Matrix3 = Map Index3 (Vector (Token, Int))

type Matrices1 = (Matrix1, Matrix1, Matrix1, Matrix1)
type Matrices2 = (Matrix2, Matrix2, Matrix2)
type Matrices3 = (Matrix3, Matrix3)

type Matrices = (Matrices1, Matrices2, Matrices3)
