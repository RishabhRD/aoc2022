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
import           Data.Char                (isDigit)
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Data.List                (transpose)
import           Data.Maybe               (catMaybes, isNothing)
import           Debug.Trace              (trace)

data Move = Move Int Int Int deriving Show

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = f xs []
    where f [] agg = [agg]
          f (y : ys) agg = if p y
                           then agg : f ys []
                           else f ys (agg ++ [y])

getCharPositions :: String -> [Int]
getCharPositions = fmap fst . filter (isDigit . snd). zip [0..]

getCharAt :: String -> Int -> Maybe Char
getCharAt str i
  | isNothing atIdx = Nothing
  | atIdx == Just ' ' = Nothing
  | otherwise = atIdx
    where
    atIdx
      | i >= length str = Nothing
      | otherwise = Just (str !! i)

parseStackLine :: String -> [Int] -> [Maybe Char]
parseStackLine = fmap . getCharAt

parseStack :: [String] -> [String]
parseStack strs = catMaybes <$> transpose  listWithMaybe
  where
  listWithMaybe = (`parseStackLine` positions) <$> init strs
  positions = getCharPositions $ last strs

readInt :: String -> Int
readInt = read

parseMoveLine :: String -> Move
parseMoveLine str = Move n (f - 1) (t - 1)
  where
  splitted = splitWhen (== ' ') str
  zipped = zip splitted [1..]
  relevant = fst <$> filter (even . snd) zipped
  [n, f, t] = fmap readInt relevant

parseMoves :: [String] -> [Move]
parseMoves  = fmap parseMoveLine

splitInput :: [String] -> [[String]]
splitInput = splitWhen (== "")

performOp :: [String] -> Move -> [String]
performOp str (Move n f t) = fmap (uncurry op) zippedList
  where
  newFrom = drop n (str !! f)
  newTo = reverse (take n (str !! f)) ++ (str !! t)
  zippedList = zip [0..] str
  op idx ele
    | idx == f = newFrom
    | idx == t = newTo
    | otherwise = ele

performOp' :: [String] -> Move -> [String]
performOp' str (Move n f t) = fmap (uncurry op) zippedList
  where
  newFrom = drop n (str !! f)
  newTo = take n (str !! f) ++ (str !! t)
  zippedList = zip [0..] str
  op idx ele
    | idx == f = newFrom
    | idx == t = newTo
    | otherwise = ele

solve :: [String] -> String
solve lines = head . transpose $ finalStack
  where
  [rawStack, rawMoves] = splitInput lines
  stack = parseStack rawStack
  moves = parseMoves rawMoves
  finalStack = foldl performOp stack moves

solve' :: [String] -> String
solve' lines = head . transpose $ finalStack
  where
  [rawStack, rawMoves] = splitInput lines
  stack = parseStack rawStack
  moves = parseMoves rawMoves
  finalStack = foldl performOp' stack moves

main :: IO ()
main = do
  content <- getContents
  let l = lines content
  print $ solve l
  print $ solve' l
