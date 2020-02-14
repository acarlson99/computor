module Poly.Term
    ( Term(..)
    , addTerm
    , subTerm
    , negateTerm
    , termReg
    , strToTerm
    , termsToStr
    )
where

import           Text.Regex.Posix

data Term = Term { termCoef :: Float, termExp :: Int }

instance Show Term where
    show (Term c 0) = show c
    show (Term 1 1) = "X"
    show (Term 1 f) = "X^" ++ show f
    show (Term c 1) = show c ++ "X"
    show (Term c f) = show c ++ "X^" ++ show f

addTerm :: Term -> Term -> Maybe Term
addTerm (Term c1 f1) (Term c2 f2) | f1 == f2  = Just $ Term (c1 + c2) f1
                                  | otherwise = Nothing

subTerm :: Term -> Term -> Maybe Term
subTerm (Term c1 f1) (Term c2 f2) | f1 == f2  = Just $ Term (c1 - c2) f1
                                  | otherwise = Nothing

negateTerm :: Term -> Term
negateTerm (Term c f) = Term (-c) f

termReg :: String
termReg =
    "\\+?(-?[[:digit:]]+(\\.?[[:digit:]]+)?)?\\*?(X(\\^([[:digit:]]+))?)?"

strToTerm :: String -> Term
strToTerm xs = Term (read' c) (read' f)
  where
    ([_, c, _, x, _, f] : _) = xs =~ termReg :: [[String]]
    read' n | n == "", x == "" = 0
            | n == ""          = 1
            | otherwise        = read n

termsToStr :: [Term] -> String
termsToStr [] = "0"
termsToStr xs = foldl f "" xs
  where
    f x y | x == ""   = show y
          | otherwise = show y ++ " + " ++ x
