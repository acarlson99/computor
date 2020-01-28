module Term
    ( Term (..)
    , addTerm
    , subTerm
    , negateTerm
    , strToTerm
    , termsToStr
    ) where

import Text.Regex.Posix

data Term = Term { coef :: Float, exp :: Int }

instance Show Term where
    show (Term c f) = show c ++ "X^" ++ show f

addTerm :: Term -> Term -> Maybe Term
addTerm (Term c1 f1) (Term c2 f2)
    | f1 == f2  = Just $ Term (c1 + c2) f1
    | otherwise = Nothing

subTerm :: Term -> Term -> Maybe Term
subTerm (Term c1 f1) (Term c2 f2)
    | f1 == f2  = Just $ Term (c1 - c2) f1
    | otherwise = Nothing

negateTerm :: Term -> Term
negateTerm (Term c f) = Term (-c) f

-- ~ NOTE: work in progress
termReg :: String
termReg = "\\+?(-?[[:digit:]]+(\\.?[[:digit:]]+)?)\\*?X\\^(-?[[:digit:]]+)"

-- strToTerm "1*X^3" :: Term 1 3
strToTerm :: String -> Term
strToTerm xs = Term (read c) (read f)
          where [[_,c,_,f]] = xs =~ "(-?[[:digit:]]+(\\.?[[:digit:]]+)?)\\*?X\\^(-?[[:digit:]]+)" :: [[String]]

termsToStr lhs = foldl f "" lhs
    where f x y | x == "" = show y
                | otherwise = show y ++ " + " ++ x
