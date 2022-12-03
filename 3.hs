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
import           Data.Array               (Array, (!))
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BSI
import           Data.Char                (isLower, ord)
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Data.List                (group, groupBy, sort)
import           Debug.Trace              (trace)
import           GHC.Base                 (join)

priority :: Char -> Int
priority x
  | isLower x = aOrd + (ord x - ord 'a')
  | otherwise = aOrd' + (ord x - ord 'A')
    where
    aOrd = 1
    aOrd' = 27

createFreqMap :: [Int] -> IntMap.IntMap Int
createFreqMap = IntMap.fromList . fmap (\x -> (head x, length x)). group . sort

filterByMap :: IntMap.IntMap Int -> [Int] -> [Int]
filterByMap = filter . flip IntMap.member

score :: [Int] -> Int
score xs = head $ filterByMap freqMap right
  where
  n = length xs
  left = take (n `div` 2) xs
  right = drop (n `div` 2) xs
  freqMap = createFreqMap left

solve :: [String] -> Int
solve = sum . fmap (score . fmap priority)

score' :: [[Int]] -> Int
score' [left, middle, right] = head realRight
  where
  leftMap = createFreqMap left
  realMiddle = filterByMap leftMap middle
  middleMap = createFreqMap realMiddle
  realRight = filterByMap middleMap right

chunk :: Int -> [a] -> [[a]]
chunk n xs =  fmap (fmap fst) grouped
  where
  indexes = replicate n =<< [1..]
  zipped = zip xs indexes
  grouped = groupBy (\x y -> snd x == snd y) zipped

solve' :: [String] -> Int
solve' xs = sum (score' <$> chunked)
  where
  plist = fmap (fmap priority) xs
  chunked = chunk 3 plist

main :: IO ()
main = do
  content <- getContents
  let l = lines content
  print $ solve l
  print $ solve' l
