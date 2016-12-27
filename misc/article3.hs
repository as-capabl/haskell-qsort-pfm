{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Criterion.Main
import Data.Int
import Data.Word
import Data.Bits (xor)
import Data.Foldable (forM_, foldl')
import Data.Traversable (forM)
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as UV
import qualified Data.Vector.Storable.Mutable as SV
import qualified Data.Vector.Mutable as BV
import qualified Data.Vector.Generic.Mutable as GV
import Control.Monad.Primitive (RealWorld)
import System.Exit (die)

import qualified List
import qualified List2
import qualified UV
import qualified BV
import qualified CXX
import qualified Data.List as DataList
import qualified Data.Vector.Algorithms.Intro as VectorArgorithms


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

verifySortImpl :: Word64 -> [IO Word64] -> IO ()
verifySortImpl parity l0 =
  do
    parity' <- go minBound 0 l0
    if not (parity == parity') then die "parity error" else return ()
  where
    go currentMax currentParity (mx:l) =
      do
        x <- mx
        if x < currentMax then die "order error" else return ()
        let !parity = currentParity `xor` x
        go x parity l

    go _ currentParity [] =
        return currentParity

verifySort :: Word64 -> [Word64] -> IO ()
verifySort parity l = verifySortImpl parity $ map return l

verifySortVec ::
    GV.MVector v Word64 =>
    Word64 -> v RealWorld Word64 -> IO ()
verifySortVec parity v =
    verifySortImpl parity $ [GV.unsafeRead v i | i <- [0 .. GV.length v - 1]]

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

    let parity = foldl' xor 0 orig

    -- Measurement
    defaultMain
      [
        bgroup "reduction-by-nf"
          [
            bench "trivial" $ nf quicksort00 orig,
            bench "accumulator" $ nf quicksortA orig,
            bench "divide" $ nf quicksortD orig,
            bench "divide+accumulator" $ nf quicksortDA orig
          ],
        bgroup "reduction-by-folding"
          [
            bench "trivial" $ whnfIO (verifySort parity $ quicksort00 orig),
            bench "accumulator" $ whnfIO (verifySort parity $ quicksortA orig),
            bench "divide" $ whnfIO (verifySort parity $ quicksortD orig),
            bench "divide+accumulator" $ whnfIO (verifySort parity $ quicksortDA orig)
          ]
      ]

