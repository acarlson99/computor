module Types
    ( Complex(..)
    , complex
    )
where

import           Data.Matrix

newtype Complex a = Complex (a,a) deriving (Eq)

instance (Floating a) => Num (Complex a) where
    Complex (a, b) + Complex (c, d) = Complex (a + c, b + d)
    Complex (a, b) - Complex (c, d) = Complex (a - c, b - d)
    Complex (a, b) * Complex (c, d) = Complex (a * c - b * d, a * d + b * c)

    abs (Complex (a, b)) = Complex (sqrt ((a ^ 2) + (b ^ 2)), 0)
    signum (Complex (a, b)) = Complex (signum a, signum b)
    fromInteger i = Complex (fromInteger i, 0)

instance Functor Complex where
    fmap f (Complex (a, b)) = Complex (f a, f b)

instance (Show a, Num a, Ord a) => Show (Complex a) where
    show (Complex (a, b))
        | a == 0    = "(" ++ show b ++ "i" ++ ")"
        | b < 0     = "(" ++ show a ++ show b ++ "i" ++ ")"
        | otherwise = "(" ++ show a ++ "+" ++ show b ++ "i" ++ ")"

complex n = Complex (0, n)
