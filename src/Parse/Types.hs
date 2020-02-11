module Parse.Types
    ( module Parse.Types
    ) where

import qualified Types as T
import Util

data Cmd = Quit
         | EvalPoly String
         | Help
         | Reset
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
               | Identifier Ident
               | Complex (T.Complex Float)
               deriving (Eq)

data Expr = Primitive' Primitive

          | Array [Expr]
          | Matrix [Expr]

          | Funcall Fcall
          | Operation (Operator, Expr, Expr)   -- 1 + 2 === + 1 2
          deriving (Eq)

data ParseTree = Expr' Expr
               | Assignment (Ident, Expr)
               | Defun (Fcall, Expr) -- (funcall, expr)

               | Command Cmd

               | Error String
               deriving (Eq)

instance Show Operator where
    show Add        = "+"
    show Sub        = "-"
    show Mult       = "*"
    show Div        = "/"
    show Exp        = "^"
    show MatrixMult = "**"
    show (Other s)  = s

instance Show Ident where
    show (Ident idn) = idn

instance Show Fcall where
    show (Fcall (idn,x:xs)) = show idn ++ "(" ++ show x ++ showSepList ',' xs ++ ")"
    show (Fcall (idn,[])) = show idn ++ "()"

instance Show Primitive where
    show (Number n) = show n
    show (Float f) = show f
    show (Identifier idn) = show idn
    show (Complex cplx) = show cplx

instance Show Expr where
    show (Primitive' prim)        = show prim
    show (Array (x:xs))           = '[' : show x  ++ showSepList ',' xs ++ "]"
    show (Array [])               = "[]"
    show (Matrix (x:xs))          = '[' : show x ++ showSepList ';' xs ++ "]"
    show (Matrix [])              = "[]"
    show (Funcall fc)             = show fc
    show (Operation (op,lhs,rhs)) = '(' : show lhs ++ ' ' : show op ++ ' ' : show rhs ++ ")"

instance Show ParseTree where
    show (Expr' xs) = show xs
    show (Assignment (idnt,expr)) = show idnt ++ " = " ++ show expr
    show (Defun (f,xs)) = show f ++ " = " ++ show xs
    show (Command cmd) = show cmd
    show (Error err) = "UNKNOWN VALUES: " ++ err
