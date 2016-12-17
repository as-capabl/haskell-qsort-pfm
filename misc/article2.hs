module Main where

import Criterion.Main
import Data.Int
import Data.Word
import Data.Foldable (forM_)
import Data.Traversable (forM)
import qualified System.Random.MWC as MWC

divideList :: Word64 -> [Word64] -> ([Word64], [Word64])
divideList x xs = foldr divide ([], []) xs
  where
    divide y (l, g)
        | y < x = (y:l, g)
        | otherwise = (l, y:g)

quicksort00 :: [Word64] -> [Word64]
quicksort00 [] = []
quicksort00 (x:xs)=
    let lt = filter (<x) xs
        gteq = filter (>=x) xs
      in
        quicksort00 lt ++ [x] ++ quicksort00 gteq

quicksortA :: [Word64] -> [Word64]
quicksortA l = quicksort' l []
  where
    quicksort' [] ys = ys
    quicksort' (x:xs) ys =
        let
            lt = filter (<x) xs
            gteq = filter (>=x) xs
            gteq' = quicksort' gteq ys
          in
            quicksort' lt (x:gteq')

quicksortD :: [Word64] -> [Word64]
quicksortD [] = []
quicksortD (x:xs)=
    let
        (lt, gteq) = divideList x xs
      in
        quicksortD lt ++ [x] ++ quicksortD gteq


quicksortDA :: [Word64] -> [Word64]
quicksortDA l = quicksort' l []
  where
    quicksort' [] ys = ys
    quicksort' (x:xs) ys =
        let
            (lt, gteq) = divideList x xs
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
        bench "accumulator" $ nf quicksortA orig,
        bench "divide" $ nf quicksortD orig,
        bench "divide+accumulator" $ nf quicksortDA orig
      ]

