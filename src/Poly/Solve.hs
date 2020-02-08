module Poly.Solve
    ( solve
    , printRes
    ) where

import Data.List
import Text.Regex.Posix

import Poly.Term
import Util

maybeToEither _ (Just a) = Right a
maybeToEither est Nothing = Left est

stripWhitespace :: String -> String
stripWhitespace = concat . words

rgxFilter :: String -> [String]
rgxFilter xs = getAllTextMatches $ stripWhitespace xs =~ termReg

-- Split expression "1 * X^3 + 2 * X^2 = X^1.5"
--             into (["1 * X^3", "2 * X^2"], ["X^1.5"])
splitExpr :: String -> Either String (String, String)
splitExpr expr = do
    let exprs = words expr
    idx <- maybeToEither "Unable to find '=' sign" $ elemIndex "=" exprs
    return (unwords $ take idx exprs, unwords $ drop (idx + 1) exprs)

sumTerms :: [Term] -> Maybe [Term]
sumTerms = foldl f (Just []) . map (\xs -> foldl (\x y -> x >>= addTerm y) (return $ Term 0 (termExp $ head xs)) xs) . sortBuckets termExp
    where f Nothing _ = Nothing
          f _ Nothing = Nothing
          f (Just x) (Just y) = Just (y:x)

simplifyExpr :: [Term] -> Maybe [Term]
simplifyExpr xs = sumTerms xs >>= (return . filter (\x -> termCoef x /= 0))
    where f (Just x) = [x]
          f Nothing = []

degree :: [Term] -> Int
degree = foldl (\x y -> max x $ termExp y) 0

quad :: Float -> Float -> Float -> [String]
quad a b c = let r = (b ** 2) - (4 * a * c)
                 f r a b
                     | r > 0 = let disc = sqrt r
                               in [ "Discriminant positive.  Two solutions:"
                                  , show (((-b) - disc) / (2*a))
                                  , show $ ((-b) + disc) / (2*a) ]
                     | r < 0 = let res = sqrt (-r)
                                   div = 2*a
                                   lhs = show $ (-b)/div
                                   rhs = show $ res/div
                               in [ "Discriminant negative.  Two solutions:"
                                  , lhs ++ "+" ++ rhs ++ "i"
                                  , lhs ++ "-" ++ rhs ++ "i" ]
                     | otherwise = [ "Discriminant zero.  One solution:"
                                   , show $ (-b) / (2*a) ]
    in f r a b

valOrZero :: [Term] -> Int -> Float
valOrZero ts e = f [t | t <- ts, termExp t == e]
    where f [] = 0
          f (x:xs) = termCoef x

runQuadratic :: [Term] -> Int -> [String]
runQuadratic ts 0 = "Degree zero.  One or zero solutions:"
                    : if valOrZero ts 0 == 0 then [ "Inf" ] else [ "None" ]
runQuadratic ts 1 = [ "Degree one.  One solution:"
                    , show $ (-(valOrZero ts 0)) / valOrZero ts 1 ]
runQuadratic ts 2 = quad (valOrZero ts 2) (valOrZero ts 1) (valOrZero ts 0)
runQuadratic _ degree = [ "Degree greater than 2.  Unable to solve" ]

solve expr = do
    let (lhs, rhs) = splitOn '=' expr
        f = map strToTerm . rgxFilter
        lhs' = foldl (\x y -> negateTerm y:x) (f lhs) (f rhs)
    simplified <- maybeToEither ("Unable to simplify expression: " ++ expr) $ simplifyExpr lhs'
    return ( termsToStr simplified ++ " = 0"
           , degree simplified
           , runQuadratic simplified $ degree simplified)

printRes (Left s) = putStrLn $ "ERROR: " ++ s
printRes (Right (a,b,c)) = do
    putStrLn $ "Simplified form: " ++ a
    putStrLn $ "Degree: " ++ show b
    mapM_ putStrLn c
