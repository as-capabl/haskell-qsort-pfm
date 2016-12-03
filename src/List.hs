
module
    List (quicksort)
where

import Data.Word
import Data.Foldable (forM_)
import Data.Traversable (forM)

import Data.Monoid


quicksort :: [Word64] -> [Word64]

quicksort [] = []
quicksort (x:xs)=
    let lt = filter (<x) xs
        gteq = filter (>=x) xs
      in
        quicksort lt ++ [x] ++ quicksort gteq

