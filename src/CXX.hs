
module
    CXX (quicksortWrote, quicksortStl)
where

import Data.Int
import Data.Word
import qualified Data.Vector.Storable.Mutable as V
import Foreign.Ptr

foreign import ccall "haskell-qsort-pfm-cxx"
  quicksort_wrote :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall "haskell-qsort-pfm-cxx"
  quicksort_stl :: Ptr Word64 -> Ptr Word64 -> IO ()

type IndType = Int

callVector ::
    (Ptr Word64 -> Ptr Word64 -> IO ()) ->
    V.IOVector Word64 -> IndType -> Int -> IO ()
callVector action v i count = V.unsafeWith v $ \p ->
    let
        beginPtr = plusPtr p (i * 8)
        endPtr = plusPtr p ((i + count) * 8)
      in
        action beginPtr endPtr

quicksortWrote :: V.IOVector Word64 -> IndType -> Int -> IO ()
quicksortWrote v i count =
    callVector quicksort_wrote v i count

quicksortStl :: V.IOVector Word64 -> IndType -> Int -> IO ()
quicksortStl v i count =
    callVector quicksort_stl v i count

