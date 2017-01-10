module Challenge where

import Data.List ( partition )

isNegative :: Int -> Bool
isNegative = (< 0)

combine :: [Int] -> [Int] -> [Int]
combine = combine' []
  where
    combine' acc [] ys = acc ++ ys
    combine' acc xs [] = acc ++ xs
    combine' acc n@(x:xs) p@(y:ys) = if x < y
                                        then combine' (acc ++ [x]) xs p
                                        else combine' (acc ++ [y]) n ys

squares :: [Int] -> [Int]
squares xs = combine negatives positives
  where
    groups = partition isNegative xs
    negatives = foldl (\ns n -> n^2:ns) [] (fst groups)
    positives = map (^2) (snd groups)
