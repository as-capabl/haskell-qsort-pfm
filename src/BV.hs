
module
    BV (quicksort)
where

import Data.Word
import Data.Int
import qualified Data.Vector.Mutable as V
import Data.Monoid


type IndType = Int

quicksort :: V.IOVector Word64 -> IndType -> IndType -> IO ()
quicksort work iStart iEnd
    | iEnd - iStart <= 1 = return ()
    | otherwise =
      do
        pivot <- V.unsafeRead work (((iEnd - iStart) `div` 2) + iStart)
        med <- divide pivot iStart (iEnd - 1)
        quicksort work iStart med
        quicksort work med iEnd
  where
    divide pivot iS' iE'
        | iS' > iE' = return iS'
        | otherwise =
          do
            lower <- shrinkLower pivot iS' iE'
            higher <- shrinkHigher pivot lower iE'
            if lower >= higher
              then do
                return lower
              else do
                swapWork lower higher
                divide pivot (lower + 1) (higher - 1) :: IO IndType

    shrinkLower pivot iS' iE'
        | iS' > iE' = return iS'
        | otherwise =
          do
            x <- V.unsafeRead work iS'
            if x >= pivot
              then
                return iS'
              else
                shrinkLower pivot (iS' + 1) iE' :: IO IndType

    shrinkHigher pivot iS' iE'
        | iS' > iE' = return iE'
        | otherwise =
          do
            x <- V.unsafeRead work iE'
            if x <= pivot
              then
                return iE'
              else
                shrinkHigher pivot iS' (iE' - 1) :: IO IndType

    swapWork i j = V.unsafeSwap work i j :: IO ()
