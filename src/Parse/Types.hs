module Parse.Types
    ( module Parse.Types
    )
where

import qualified Complex                       as C
import           Util

data Cmd = Quit
         | EvalPoly String
         | Help
         | Reset
         | Dump
         deriving (Show,Eq)

newtype Ident = Ident String deriving (Eq)

newtype Fcall = Fcall (Ident, [Expr]) deriving (Eq)

data Operator = Add
               | Sub
               | Mult
               | Div
               | Exp
               | Mod
               | MatrixMult
               | Other String
               deriving (Eq)

data Primitive = Number Int
               | Float Float
               | Complex (C.Complex Float)
               deriving (Eq)

data Expr = Primitive' Primitive
          | Identifier Ident

          | Array [Expr]
          | Matrix [Expr]

          | Funcall Fcall                       -- f(a,b)
          | Operation (Operator, Expr, Expr)    -- 1 + 2 === + 1 2
          deriving (Eq)

data ParseTree = Expr' Expr
               | Assignment (Ident, Expr)
               | Defun (Ident, [Ident], Expr)   -- (funcall, expr)

               | Command Cmd

               | Error String
               | EOL
               deriving (Eq)

instance Num Primitive where
    Number  n + Number  n' = Number $ n + n'
    Float   f + Float   f' = Float $ f + f'
    Number  n + Float   f  = Float $ fromIntegral n + f
    Complex c + Complex c' = Complex $ c + c'
    Complex c + Number  n  = Complex $ fromIntegral n + c
    Complex c + Float   f  = Complex $ c + C.Complex (f, 0)
    b         + a          = a + b

    Number  n - Number  n' = Number $ n - n'
    Float   f - Float   f' = Float $ f - f'
    Number  n - Float   f  = Float $ fromIntegral n - f
    Float   f - Number  n  = Float $ f - fromIntegral n
    Complex c - Complex c' = Complex $ c - c'
    Complex c - Number  n  = Complex $ c - fromIntegral n
    Number  n - Complex c  = Complex $ fromIntegral n - c
    Complex c - Float   f  = Complex $ c - C.Complex (f, 0)
    Float   f - Complex c  = Complex $ C.Complex (f, 0) - c

    Number  n * Number  n' = Number $ n * n'
    Float   f * Float   f' = Float $ f * f'
    Number  n * Float   f  = Float $ fromIntegral n * f
    Complex c * Complex c' = Complex $ c * c'
    Complex c * Number  n  = Complex $ fromIntegral n * c
    Complex c * Float   f  = Complex $ c * C.Complex (f, 0)
    b         * a          = a * b

    abs (Number  a) = Number $ abs a
    abs (Float   f) = Float $ abs f
    abs (Complex c) = Complex $ abs c

    signum (Number  a) = Number $ signum a
    signum (Float   a) = Float $ signum a
    signum (Complex a) = Complex $ signum a

    fromInteger i = Number (fromInteger i)

primToComplex :: Primitive -> C.Complex Float
primToComplex (Number  p) = C.Complex (fromIntegral p, 0)
primToComplex (Float   f) = C.Complex (f, 0)
primToComplex (Complex c) = c

instance Show Operator where
    show Add        = "+"
    show Sub        = "-"
    show Mult       = "*"
    show Div        = "/"
    show Exp        = "^"
    show MatrixMult = "**"
    show Mod        = "%"
    show (Other s)  = s

instance Show Ident where
    show (Ident idn) = idn

instance Show Fcall where
    show (Fcall (idn, x : xs)) =
        show idn ++ "(" ++ show x ++ showSepList ", " xs ++ ")"
    show (Fcall (idn, [])) = show idn ++ "()"

instance Show Primitive where
    show (Number  n   ) = show n
    show (Float   f   ) = show f
    show (Complex cplx) = show cplx

instance Show Expr where
    show (Primitive' prim    ) = show prim
    show (Identifier idn     ) = show idn
    show (Array      (x : xs)) = '[' : show x ++ showSepList ", " xs ++ "]"
    show (Array      []      ) = "[]"
    show (Matrix     (x : xs)) = '[' : show x ++ showSepList "; " xs ++ "]"
    show (Matrix     []      ) = "[]"
    show (Funcall    fc      ) = show fc
    show (Operation (op, lhs, rhs)) =
        '(' : show lhs ++ ' ' : show op ++ ' ' : show rhs ++ ")"

instance Show ParseTree where
    show (Expr'      xs          ) = show xs
    show (Assignment (idnt, expr)) = show idnt ++ " = " ++ show expr
    show (Defun      (f, [], rhs)) = show f ++ "( ) = " ++ show rhs
    show (Defun (f, x : xs, rhs)) =
        show f ++ "( " ++ show x ++ showSepList " , " xs ++ " ) = " ++ show rhs
    show (Command cmd) = show cmd
    show (Error   err) = "UNKNOWN VALUES: " ++ err
    show EOL           = "EOL"
