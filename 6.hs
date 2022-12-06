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
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BSI
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Debug.Trace              (trace)


getIdx :: Int -> String -> Int
getIdx k str' = proc 0 0 initMap
  where
  n = length str'
  str = listArray (0, n - 1) str'
  initMap = HashMap.empty
  insertSt high st = HashMap.insert highEle (highFreq + 1) st
    where
    highEle = str ! high
    highFreq = HashMap.findWithDefault 0 highEle st
  deleteSt low high st
    | highFreq > 1 = deleteSt (low + 1) high newSt
    | otherwise = (low, st)
    where
    lowFreq = st HashMap.! (str ! low)
    highFreq = st HashMap.! (str ! high)
    newSt = HashMap.insert (str ! low) (lowFreq - 1) st
  proc low high st = ans
    where
    ans
      | high - newLow + 1 == k = high  + 1
      | otherwise = proc newLow (high + 1) deletedSt
    insertedSt = insertSt high st
    (newLow, deletedSt) = deleteSt low high insertedSt


solve :: String -> Int
solve = getIdx 4

solve' :: String -> Int
solve' = getIdx 14

main :: IO ()
main = do
 str <- getLine
 print $ solve str
 print $ solve' str
