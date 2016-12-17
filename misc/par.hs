{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Control.Parallel
import Control.DeepSeq
import Data.Int
import Data.Word
import Data.Foldable (forM_)
import Data.Traversable (forM)
import qualified System.Random.MWC as MWC


quicksort00 :: [Word64] -> [Word64]
quicksort00 [] = []
quicksort00 (x:xs)=
    let lt = filter (<x) xs
        gteq = filter (>=x) xs
      in
        quicksort00 lt ++ [x] ++ quicksort00 gteq

quicksort10 :: [Word64] -> [Word64]
quicksort10 l = quicksort' l
  where
    quicksort' [] = []
    quicksort' (x:xs) =
        let
            divide y rest =
              let
                  !(l, g) = rest
                in
                  if y < x then (y:l, g) else (l, y:g)
            (lt, gteq) = foldr divide ([], []) xs
            gteq' = quicksort' gteq
          in
            deepseq gteq `par` quicksort' lt ++ (x:gteq')

quicksort11 :: [Word64] -> [Word64]
quicksort11 l = quicksort' l []
  where
    quicksort' [] ys = ys
    quicksort' (x:xs) ys =
        let
            divide y rest =
              let
                  !(l, g) = rest
                in
                  if y < x then (y:l, g) else (l, y:g)
            (lt, gteq) = foldr divide ([], []) xs
            gteq' = quicksort' gteq ys
          in
            quicksort' lt (x:gteq')



main :: IO ()
main =
  do
    let numElems = 500000

    -- Preparation
    orig <- MWC.withSystemRandom $ \gen ->
      do
        forM [0 .. numElems - 1] $ \i ->
          do
            MWC.uniform gen :: IO Word64

    -- Measurement
    defaultMain
      [
        bench "trivial" $ nf quicksort00 orig,
        bench "divide" $ nf quicksort10 orig,
        bench "divide+accumulator" $ nf quicksort11 orig
      ]

