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
import           Data.Char                (isDigit, ord)
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Debug.Trace              (trace)

readInt :: String -> (Int, String)
readInt = readInt' 0
  where
  readInt' cur (x : xs)
    | isDigit x = readInt' (cur * 10 + (ord x - ord '0')) xs
    | otherwise = (cur, x : xs)
  readInt' cur [] = (cur, [])

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine str = ((first, second), (third, fourth))
  where
  (first, strFirst) = readInt str
  (second, strSecond) = readInt (tail strFirst)
  (third, strThird) = readInt (tail strSecond)
  (fourth, _) = readInt (tail strThird)

doFullyContains :: (Int, Int) -> (Int, Int) -> Bool
doFullyContains (a, b) (c, d) = a <= c && b >= d

doOverlap :: (Int, Int) -> (Int, Int) -> Bool
doOverlap (a, b) (c, d)
  | a <= c = b >= c
  | otherwise = d >= a

composeOr :: (t -> Bool) -> (t -> Bool) -> t -> Bool
composeOr f g x = f x || g x

solve :: [String] -> Int
solve lst = length eligibleMembers
  where
  eligibleMembers = filter (composeOr (uncurry (flip doFullyContains)) (uncurry doFullyContains)) . fmap parseLine $ lst


solve' :: [String] -> Int
solve' =  length . filter (uncurry doOverlap) . fmap parseLine

main :: IO ()
main = do
  content <- getContents
  let l = lines content
  print $ solve l
  print $ solve' l
