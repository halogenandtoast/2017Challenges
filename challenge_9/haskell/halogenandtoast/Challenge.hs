module Challenge where

import Data.List ( partition )

isNegative :: Int -> Bool
isNegative = (< 0)

tmap :: ((a -> c), (b -> d)) -> (a, b) -> (c, d)
tmap (f, g) (x, y) = (f x, g y)

combine :: ([Int], [Int]) -> [Int]
combine (f, s) = combine' [] f s
  where
    combine' acc [] ys = acc ++ ys
    combine' acc xs [] = acc ++ xs
    combine' acc n@(x:xs) p@(y:ys) = if x < y
                                        then combine' (acc ++ [x]) xs p
                                        else combine' (acc ++ [y]) n ys

squares :: [Int] -> [Int]
squares = combine . groups
  where
    partitions = partition isNegative
    groups = tmap (reverseSquare, square) . partitions
    reverseSquare = foldl (\xs x -> x^2:xs) []
    square = map (^2)
