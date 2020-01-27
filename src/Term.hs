module Term
    ( Term (..)
    , addTerm
    , subTerm
    ) where

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
