module SolvePoly
    ( solvePoly
    , splitExpr
    , rgxFilter
    , splitOn
    ) where

import Data.List
import Text.Regex.Posix

import Term

-- data Reduced = String deriving (Show)
-- data Degree = Float deriving (Show)
-- data Solution = Float deriving (Show)

-- data PolyAnswer = PolyAnswer Reduced Degree Solution deriving (Show)
data PolyAnswer = PolyAnswer String Float Float deriving (Show)

-- solvePoly :: String -> PolyAnswer
-- solvePoly st = do
--     let exprs = words st
--     eqidx <- elemIndex "=" exprs
--     PolyAnswer "A" 4.0 4.0

maybeToEither _ (Just a) = Right a
maybeToEither est Nothing = Left est

stripWhitespace :: String -> String
stripWhitespace = foldl (++) "" . words

-- "1*X^3 + 2*X^7" =~ "[[:digit:]]+\\*X\\^[[:digit:]]+" :: [[String]]

-- ~ rgxFilter :: String -> [[String]]
-- ~ rgxFilter xs = filter (\a -> a /= ["","","","",""]) $ xs =~ "(([[:digit:]]+)\\*)?(X\\^([[:digit:]])+)?"
rgxFilter :: String -> [String]
rgxFilter xs = getAllTextMatches $ stripWhitespace xs =~ "[+\\-]? ?[[:digit:]]+(\\.[[:digit:]]+)?\\*X\\^-?[[:digit:]]+"

-- Split expression "1 * X^3 + 2 * X^2 = X^1.5"
--             into (["1 * X^3", "2 * X^2"], ["X^1.5"])
splitExpr :: String -> Either String (String, String)
splitExpr expr = do
    let exprs = words expr
    idx <- maybeToEither "Unable to find '=' sign" $ elemIndex "=" exprs
    return (unwords $ take idx exprs, unwords $ drop (idx + 1) exprs)

splitOn :: Char -> String -> (String, String)
splitOn _ [] = ("","")
splitOn c (x:xs)
    | c == x = ("",xs)
    | otherwise = (x:lhs,rhs)
                  where (lhs,rhs) = splitOn c xs

-- solvePoly :: String -> Either String PolyAnswer
solvePoly expr = do
    -- ~ (lhs, rhs) <- splitExpr expr
    let (lhs, rhs) = splitOn '=' expr
        f = map strToTerm . rgxFilter
    return (f lhs, f rhs)
