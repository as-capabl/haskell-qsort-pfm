module Main where

import Criterion.Main
import Data.Int
import Data.Word
import Data.Foldable (forM_)
import Data.Traversable (forM)
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as UV
import qualified Data.Vector.Storable.Mutable as SV
import qualified Data.Vector.Mutable as BV

import qualified List
import qualified List2
import qualified UV
import qualified BV
import qualified CXX


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

    uv <- UV.new numElems :: IO (UV.IOVector Word64)
    forM_ ([0 .. numElems - 1] `zip` orig) $ \(i, x) ->
      do
        UV.write uv i x :: IO ()

    bv <- BV.new numElems :: IO (BV.IOVector Word64)
    forM_ ([0 .. numElems - 1] `zip` orig) $ \(i, x) ->
      do
        BV.write bv i x :: IO ()

    cw <- SV.new numElems :: IO (SV.IOVector Word64)
    forM_ ([0 .. numElems - 1] `zip` orig) $ \(i, x) ->
      do
        SV.write cw i x :: IO ()

    -- Measurement
    defaultMain
      [
        bgroup "list"
          [
            bench "trivial" $ nf List.quicksort orig,
            bench "improved" $ nf List2.quicksort orig
          ],
        bgroup "vector"
          [
            bench "boxed" $ whnfIO (BV.quicksort bv 0 numElems),
            bench "unboxed" $ whnfIO (UV.quicksort uv 0 numElems)
          ],
        bgroup "c++"
          [
            bench "wrote" $ whnfIO (CXX.quicksort cw 0 numElems)
          ]
      ]

