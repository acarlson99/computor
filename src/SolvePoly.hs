module SolvePoly
    ( solvePoly
    , splitExpr
    , simplifyExpr
    , rgxFilter
    ) where

import Data.List
import Text.Regex.Posix

import Term
import Util

-- data Reduced = String deriving (Show)
-- data Degree = Float deriving (Show)
-- data Solution = Float deriving (Show)

-- data PolyAnswer = PolyAnswer Reduced Degree Solution deriving (Show)
data PolyAnswer = PolyAnswer String Float Float deriving (Show)

maybeToEither _ (Just a) = Right a
maybeToEither est Nothing = Left est

stripWhitespace :: String -> String
stripWhitespace = concat . words

-- "1*X^3 + 2*X^7" =~ "[[:digit:]]+\\*X\\^[[:digit:]]+" :: [[String]]
rgxFilter :: String -> [String]
rgxFilter xs = getAllTextMatches $ stripWhitespace xs =~ termReg

-- Split expression "1 * X^3 + 2 * X^2 = X^1.5"
--             into (["1 * X^3", "2 * X^2"], ["X^1.5"])
splitExpr :: String -> Either String (String, String)
splitExpr expr = do
    let exprs = words expr
    idx <- maybeToEither "Unable to find '=' sign" $ elemIndex "=" exprs
    return (unwords $ take idx exprs, unwords $ drop (idx + 1) exprs)

simplifyExpr :: [Term] -> [Term]
simplifyExpr = filter (\x -> termCoef x /= 0) . foldl (\x y -> f y ++ x) [] . map (\xs -> foldl (\x y -> x >>= addTerm y) (return $ Term 0 (termExp $ head xs)) xs) . sortBuckets termExp
    where f (Just x) = [x]
          f Nothing = []

-- solvePoly :: String -> Either String PolyAnswer
solvePoly expr = do
    -- ~ (lhs, rhs) <- splitExpr expr
    let (lhs, rhs) = splitOn '=' expr
        f = map strToTerm . rgxFilter
        lhs' = foldl (\x y -> negateTerm y:x) (f lhs) (f rhs)
    (f lhs, f rhs, lhs')
