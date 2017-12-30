module Main where

import Criterion.Main
import Data.Int
import Data.Word
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Control.DeepSeq
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as UV
import qualified Data.Vector.Storable.Mutable as SV
import qualified Data.Vector.Mutable as BV
import qualified Data.Vector.Generic as G
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
    origU <- MWC.withSystemRandom $ \gen ->
      do
        U.replicateM numElems (MWC.uniform gen :: IO Word64)
    let origL = U.toList origU
        origS = S.fromListN numElems origL
    (origL, origS) `deepseq` return ()

    -- Measurement
    defaultMain
      [
        bgroup "List -> Vector"
          [
            bench "trivial(U)" $ whnfIO $
                return (List.quicksort origL) >>= listToVec numElems,
            bench "c++_STL" $ whnfIO $
                listToVecS numElems origL >>= \sv -> CXX.quicksortStl sv 0 numElems
          ],
        bgroup "Vector -> Vector"
          [
            bench "trivial(U)" $ whnfIO $
                return (List.quicksort $ U.toList origU) >>= listToVec numElems,
            bench "c++_STL" $ whnfIO $
                thaw origS >>= \sv -> CXX.quicksortStl sv 0 numElems
          ],
        bgroup "Vector (in-place)"
          [
            env (return $ U.force origU) $ \copyU -> bench "trivial(U)" $ whnfIO $
              do
                uv2 <- listToVec numElems $ List.quicksort (U.toList copyU)
                uv <- U.unsafeThaw copyU
                UV.unsafeCopy uv uv2,
            env (S.thaw origS) $ \sv -> bench "c++_STL" $ whnfIO $
              do
                CXX.quicksortStl sv 0 numElems
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

