
module
    List2 (quicksort)
where

import Data.Word

import Data.Monoid


quicksort :: [Word64] -> [Word64]
quicksort l = quicksort' l []
  where
    quicksort' [] ys = ys
    quicksort' (x:xs) ys =
        let
            divide y (l, g)
                | y < x = (y:l, g)
                | otherwise = (l, y:g)
            (lt, gteq) = foldr divide ([], []) xs
            gteq' = quicksort' gteq ys
          in
            quicksort' lt (x:gteq')
