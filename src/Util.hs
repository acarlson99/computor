module Util
    ( module Util
    )
where

splitOn :: Char -> String -> (String, String)
splitOn _ [] = ("", "")
splitOn c (x : xs) | c == x    = ("", xs)
                   | otherwise = (x : lhs, rhs)
    where (lhs, rhs) = splitOn c xs

sortBuckets :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortBuckets _ [] = []
sortBuckets f (x : xs) =
    let small = sortBuckets f [ y | y <- xs, f y < f x ]
        big   = sortBuckets f [ y | y <- xs, f y > f x ]
    in  small ++ (x : [ y | y <- xs, f y == f x ]) : big

showSepList :: Show a => String -> [a] -> String
showSepList sep (x : xs) = sep ++ show x ++ showSepList sep xs
showSepList _   []       = ""

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _     (Just a) = Right a
maybeToEither errst Nothing  = Left errst
