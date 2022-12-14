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
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Debug.Trace              (trace)

data Choice = Rock | Paper | Scissors deriving Eq

data Result = Win | Draw | Lose

choiceScore :: Choice -> Int
choiceScore Rock     = 1
choiceScore Paper    = 2
choiceScore Scissors = 3

resultScore :: Result -> Int
resultScore Win  = 6
resultScore Draw = 3
resultScore Lose = 0

opponentChoice :: Char -> Choice
opponentChoice 'A' = Rock
opponentChoice 'B' = Paper
opponentChoice 'C' = Scissors

playerChoice :: Char -> Choice
playerChoice 'X' = Rock
playerChoice 'Y' = Paper
playerChoice 'Z' = Scissors

forWin :: Choice -> Choice
forWin Rock     = Paper
forWin Paper    = Scissors
forWin Scissors = Rock

calcResult :: Choice -> Choice -> Result
calcResult op pl
  | op == pl = Draw
  | forWin op == pl = Win
  | otherwise = Lose

toResult :: Char -> Result
toResult 'X' = Lose
toResult 'Y' = Draw
toResult 'Z' = Win

toChoice :: Choice -> Result -> Choice
toChoice op Win  = forWin op
toChoice op Lose = forWin . forWin $ op
toChoice op Draw = op

score :: (Choice, Choice) -> Int
score (op, pl) = choiceScore pl + resultScore (calcResult op pl)

parseLine :: String -> (Choice, Choice)
parseLine str = (opponentChoice (head str), playerChoice (str !! 2))

parseLine' :: String -> (Choice, Result)
parseLine' str = (opponentChoice (head str), toResult (str !! 2))

score' :: (Choice, Result) -> Int
score' (op, res) = choiceScore (toChoice op res) + resultScore res

solve :: [String] -> Int
solve = sum . fmap (score . parseLine)

solve' :: [String] -> Int
solve' = sum . fmap (score' . parseLine')

main :: IO ()
main = do
  content <- getContents
  let l = lines content
  print $ solve l
  print $ solve' l
