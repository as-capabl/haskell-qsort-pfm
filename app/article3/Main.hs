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
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Fusion.Bundle as Bun

import qualified List
import qualified List2
import qualified UV
import qualified BV
import qualified CXX
import qualified Data.List as DataList
import qualified Data.Vector.Algorithms.Intro as VectorArgorithms

main :: IO ()
main =
  do
    let numElems = 1000000

    -- Preparation
    orig <- MWC.withSystemRandom $ \gen ->
      do
        forM [0 .. numElems - 1] $ \i ->
          do
            MWC.uniform gen :: IO Word64

    -- Measurement
    defaultMain
      [
        bgroup "list"
          [
            bench "trivial(U)" $ whnfIO (return (List.quicksort orig) >>= listToVec numElems),
            bench "trivial(S)" $ whnfIO (return (List.quicksort orig) >>= listToVecS numElems),
            bench "improved" $ whnfIO (return (List2.quicksort orig) >>= listToVec numElems)
          ],
        bgroup "vector"
          [
            bench "unboxed" $ whnfIO (listToVec numElems orig >>= \uv -> UV.quicksort uv 0 numElems)
          ],
        bgroup "c++"
          [
            bench "wrote" $ whnfIO (listToVecS numElems orig >>= \cw -> CXX.quicksortStl cw 0 numElems)
          ],
        bgroup "library"
          [
            bench "Data.List" $ whnfIO (return (DataList.sort orig) >>= listToVec numElems),
            bench "Vector.Argorithms" $ whnfIO (listToVec numElems orig >>= VectorArgorithms.sort),
            bench "c++_STL" $ whnfIO (listToVecS numElems orig >>= \sv -> CXX.quicksortStl sv 0 numElems)
          ]
      ]

listToVec :: Int -> [Word64] -> IO (UV.IOVector Word64)
listToVec n l =
    -- U.unsafeThaw $ U.fromListN n l
    V.unstream $ Bun.fromListN n l

listToVecS :: Int -> [Word64] -> IO (SV.IOVector Word64)
listToVecS n l =
    -- S.unsafeThaw $ S.fromListN n l
    V.unstream $ Bun.fromListN n l
