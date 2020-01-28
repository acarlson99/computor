module Util
    ( splitOn
    , sortBuckets
    ) where

splitOn :: Char -> String -> (String, String)
splitOn _ [] = ("","")
splitOn c (x:xs)
    | c == x = ("",xs)
    | otherwise = (x:lhs,rhs)
                  where (lhs,rhs) = splitOn c xs

sortBuckets :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortBuckets f [] = []
sortBuckets f (x:xs) =
    let small = sortBuckets f [y | y <- xs, f y < f x]
        big = sortBuckets f [y | y <- xs, f y > f x]
    in small ++ (x:[y | y <- xs, f y == f x]) : big
