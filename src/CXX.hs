
module
    CXX (quicksort)
where

import Data.Int
import Data.Word
import qualified Data.Vector.Storable.Mutable as V
import Foreign.Ptr

foreign import ccall "haskell-qsort-pfm-cxx"
  quicksort_ :: Ptr Word64 -> Ptr Word64 -> IO ()

type IndType = Int

quicksort :: V.IOVector Word64 -> IndType -> IndType -> IO ()
quicksort v i count = V.unsafeWith v $ \p ->
    let
        beginPtr = plusPtr p (i * 8)
        endPtr = plusPtr p ((i + count) * 8)
      in
        quicksort_ beginPtr endPtr

