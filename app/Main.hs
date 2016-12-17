{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Vector.Generic.Mutable as GV
import Control.Monad.Primitive (RealWorld)

import qualified List
import qualified List2
import qualified UV
import qualified BV
import qualified CXX
import qualified Data.List as DataList
import qualified Data.Vector.Algorithms.Intro as VectorArgorithms

copyList ::
    GV.MVector v a =>
    v RealWorld a -> [a] -> IO ()
copyList v l =
  do
    let numElems = length l
    forM_ ([0 .. numElems - 1] `zip` l) $ \(i, x) ->
      do
        GV.write v i x

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
    bv <- BV.new numElems :: IO (BV.IOVector Word64)
    cw <- SV.new numElems :: IO (SV.IOVector Word64)
    va <- UV.new numElems :: IO (UV.IOVector Word64)
    stl <- SV.new numElems :: IO (SV.IOVector Word64)

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
            env (copyList bv orig) $ \_ ->
                 bench "boxed" $ whnfIO $ BV.quicksort bv 0 numElems,
            env (copyList uv orig) $ \_ ->
                bench "unboxed" $ whnfIO $ UV.quicksort uv 0 numElems
          ],
        bgroup "c++"
          [
            env (copyList cw orig) $ \_ ->
                 bench "wrote" $ whnfIO $ CXX.quicksortWrote cw 0 numElems
          ],
        bgroup "library"
          [
            bench "Data.List" $ nf DataList.sort orig,
            env (copyList va orig) $ \_ ->
                bench "Vector.Argorithms" $ whnfIO $ VectorArgorithms.sort va,
            env (copyList stl orig) $ \_ ->
                 bench "wrote" $ whnfIO $ CXX.quicksortStl stl 0 numElems
          ]
      ]

