-- pragmas.hs  {{{
-- vim: foldmethod=marker
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- pragmas.hs }}}
module Main where

import           Control.Monad            (replicateM_)
import           Data.Array               (Array, listArray, (!))
import           Data.Bool                (bool)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BSI
import           Data.Char                (ord)
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Data.List
import           Debug.Trace              (trace)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

nextGE :: [Int] -> [Int]
nextGE lst = init $ fst <$> scanr (flip op) (0, []) xs
  where
    n = length lst
    xs = enumerate lst
    op :: (Int, [(Int, Int)]) -> (Int, Int) -> (Int, [(Int, Int)])
    op (_, []) (i, h) = (n - i - 1, [(i, h)])
    op (res, (j, hj) : stack) (i, hi)
      | hj >= hi = (j - i, (i, hi) : (j, hj) : stack)
      | otherwise = op (res, stack) (i, hi)

toInt :: Char -> Int
toInt c = ord c - ord '0'

isVisible :: [Int] -> [Bool]
isVisible heights = zipWith (>) heights . scanl max (-1) $ heights

makeDirectionOp f = (leftOp, rightOp, upOp, downOp)
  where
  leftOp = fmap f
  rightOp = fmap reverse . leftOp. fmap reverse
  upOp = transpose . leftOp . transpose
  downOp = transpose . rightOp. transpose

makeDirectionMatrix :: ([a] -> [d]) -> [[a]] -> (d -> d -> d -> d -> e) -> [[e]]
makeDirectionMatrix op matrix zipStrategy = zipWith4 zipOp left right up down
  where
  (leftOp, rightOp, upOp, downOp) = makeDirectionOp op
  left = leftOp matrix
  right = rightOp matrix
  up = upOp matrix
  down = downOp matrix
  zipOp = zipWith4 zipStrategy

solve :: [[Int]] -> Int
solve matrix = sum (fmap sum visibleGrid)
  where
  visibleGrid = fmap (fmap $ bool 0 1) zipped
  (leftOp, rightOp, upOp, downOp) = makeDirectionOp isVisible
  zipped = makeDirectionMatrix isVisible matrix zipOp
  zipOp a b c d = a || b || c || d

solve' :: [[Int]] -> Int
solve' matrix = maximum (fmap maximum zipped)
  where
  zipped = makeDirectionMatrix nextGE matrix zipOp
  zipOp a b c d = a * b * c * d

main :: IO ()
main = do
  c <- getContents
  let l = lines c
  let matrix = fmap (fmap toInt) l
  print $ solve matrix
  print $ solve' matrix
