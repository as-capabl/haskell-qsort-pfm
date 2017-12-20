{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Criterion.Main
import Data.Int
import Data.Word
import Data.Bits (xor)
import Data.Foldable (forM_, foldl')
import Data.Traversable (forM)
import Data.IORef
import qualified System.Random.MWC as MWC
import System.Exit (die)
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

verifySortImpl :: Word64 -> [IO Word64] -> IO ()
verifySortImpl parity l0 =
  do
    parity' <- go minBound 0 l0
    if parity /= parity' then die "parity error" else return ()
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

makeData ::
    Int ->
    IO ([Word64], Word64)
makeData numElems =
  do
    orig <- MWC.withSystemRandom $ \gen ->
      do
        forM [0 .. numElems - 1] $ \i ->
          do
            MWC.uniform gen :: IO Word64
    let !parity = foldl' xor 0 orig
    return (orig, parity)

main :: IO ()
main =
  do
    let numElems = 500000

    -- Preparation
    (orig, parity) <- makeData numElems

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
            env (makeData numElems) $ \(l, p) ->
                bench "trivial" $ whnfIO (verifySort p $ List.quicksort l),
            env (return orig) $ \l ->
            bench "improved" $ whnfIO (verifySort parity $ List2.quicksort l)
          ],
        bgroup "vector"
          [
            env (copyList bv orig) $ \_ ->
                 bench "boxed" $ whnfIO $ BV.quicksort bv 0 numElems >> verifySortVec parity bv,
            env (copyList uv orig) $ \_ ->
                bench "unboxed" $ whnfIO $ UV.quicksort uv 0 numElems >> verifySortVec parity uv
          ],
        bgroup "c++"
          [
            env (copyList cw orig) $ \_ ->
                 bench "wrote" $ whnfIO $ CXX.quicksortWrote cw 0 numElems >> verifySortVec parity cw
          ],
        bgroup "library"
          [
            env (return orig) $ \l ->
                bench "Data.List" $ whnfIO (verifySort parity $ DataList.sort l),
            env (copyList va orig) $ \_ ->
                bench "Vector.Argorithms" $ whnfIO $ VectorArgorithms.sort va >> verifySortVec parity va,
            env (copyList stl orig) $ \_ ->
                 bench "c++_STL" $ whnfIO $ CXX.quicksortStl stl 0 numElems >> verifySortVec parity stl
          ]
      ]

