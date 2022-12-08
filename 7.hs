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
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Data.List                (groupBy, sort)
import           Debug.Trace              (trace)

readInt :: String -> Int
readInt = read

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = f xs []
    where f [] agg = [agg]
          f (y : ys) agg = if p y
                           then agg : f ys []
                           else f ys (agg ++ [y])

data Entry = LSEntry
             | CDEntry String
             | FileEntry String Int
             | DirectoryEntry String

makeDirectoryName :: String -> String -> String
makeDirectoryName cur ".." = reverse . dropWhile (/= '/') . tail . reverse $ cur
makeDirectoryName cur "/"  = "/"
makeDirectoryName cur name = cur ++ name ++ "/"

makeFileName :: String -> String -> String
makeFileName cur name = cur ++ name

parseLine :: String -> Entry
parseLine str
  | head splitted == "$" && (splitted !! 1) == "ls" = LSEntry
  | head splitted == "$" && (splitted !! 1) == "cd" = CDEntry $ splitted !! 2
  | head splitted == "dir" = DirectoryEntry $ splitted !! 1
  | otherwise = FileEntry (splitted !! 1) (readInt . head $ splitted)
  where
  splitted = splitWhen (== ' ') str

makeGraph :: [Entry] -> HashMap.HashMap String [String]
makeGraph = snd . foldl op ("/", HashMap.empty)
  where
  op (cur, st) LSEntry        = (cur, st)
  op (cur, st) (CDEntry name) = (makeDirectoryName cur name, st)
  op (cur, st) (DirectoryEntry name) = (cur, HashMap.insert cur (curList ++ [makeDirectoryName cur name]) st)
    where curList = HashMap.findWithDefault [] cur st
  op (cur, st) (FileEntry name _) = (cur, HashMap.insert cur (curList ++ [makeFileName cur name]) st)
    where curList = HashMap.findWithDefault [] cur st

makeFileSize :: [Entry] -> HashMap.HashMap String Int
makeFileSize = snd . foldl op ("/", HashMap.empty)
  where
  op (cur, st) LSEntry        = (cur, st)
  op (cur, st) (CDEntry name) = (makeDirectoryName cur name, st)
  op (cur, st) (FileEntry name size) = (cur, HashMap.insert (makeFileName cur name) size st)
  op (cur, st) _ = (cur, st)

getSize :: String -> HashMap.HashMap String [String] -> HashMap.HashMap String Int -> (HashMap.HashMap String Int, Int)
getSize dir graph sizes
  | HashMap.member dir sizes = (sizes, sizes HashMap.! dir)
  | otherwise = (HashMap.insert dir cSize newMap, cSize)
    where
    (newMap, cSize) = foldl op (sizes, 0) (graph HashMap.! dir)
    op (curMap, size) entry = (newMap, size + curSize)
      where
      (newMap, curSize) = getSize entry graph curMap

solve :: [String] -> Int
solve str = sum . fmap snd . filter (uncurry cond) . HashMap.toList . fst $ sizes
  where
  entries = fmap parseLine str
  graph = makeGraph entries
  sizes = getSize "/" graph $ makeFileSize entries
  cond k v = last k == '/' && v <= 100000

solve' :: [String] -> Int
solve' str = head . dropWhile ((< 30000000) . (initFree+)) $ dirSizes
  where
  entries = fmap parseLine str
  graph = makeGraph entries
  sizes = getSize "/" graph . makeFileSize $ entries
  cond k v = last k == '/'
  initFree = 70000000 - snd sizes
  dirSizes = sort . fmap snd . filter (uncurry cond) . HashMap.toList . fst $ sizes

main :: IO ()
main = do
  c <- getContents
  let l = lines c
  print $ solve l
  print $ solve' l
