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

newtype Ident = Ident String deriving (Eq,Show)

newtype Fcall = Fcall (Ident, [Expr]) deriving (Eq,Show)

data Operator = Add
               | Sub
               | Mult
               | Div
               | Exp
               | Mod
               | MatrixMult
               | Other String
               deriving (Eq,Show)

data Primitive = Number Int
           | Float Float
           | Identifier Ident
           | Complex (T.Complex Float)
           deriving (Eq,Show)

data Expr = Primitive' Primitive
          | Array [Expr]
          | Matrix [Expr]

          | Funcall Fcall
          | Operation (Operator, Expr, Expr)   -- 1 + 2 === + 1 2
          deriving (Eq,Show)

data ParseTree = Expr' Expr
               | Assignment (Ident, Expr)
               | Defun (Fcall, Expr) -- (funcall, expr)

               | Command Cmd

               | Error String
               deriving (Eq,Show)

-- ~ instance Show Operator where
    -- ~ show Add        = "+"
    -- ~ show Sub        = "-"
    -- ~ show Mult       = "*"
    -- ~ show Div        = "/"
    -- ~ show Exp        = "^"
    -- ~ show MatrixMult = "**"
    -- ~ show (Other s)  = s

-- ~ instance Show ParseTree where
    -- ~ show (Number n) = show n
    -- ~ show (Float n) = show n
    -- ~ show (Identifier s) = s
    -- ~ show (Complex c) = show c
    -- ~ show (Operation (op,lhs,rhs)) = '(' : show lhs ++ ' ' : show op ++ ' ' : show rhs ++ ")"

    -- ~ show (Array (x:xs)) = '[' : show x  ++ showSepList ',' xs ++ "]"
    -- ~ show (Array []) = "[ ]"
    -- ~ show (Matrix (x:xs)) = '[' : show x ++ showSepList ';' xs ++ "]"
    -- ~ show (Matrix []) = "[ ]"
    -- ~ show (Funcall (f,x:xs)) = show f ++ '(' : show x ++ showSepList ',' xs ++ ")"
    -- ~ show (Funcall (f,[])) = show f ++ "( )"

    -- ~ show (Assignment (f,xs)) = show f ++ " = " ++ show xs
    -- ~ show (Defun (f,xs)) = show f ++ " = " ++ show xs

    -- ~ show (Command cmd) = show cmd

    -- ~ show (Error s) = "UNKNOWN VALUES: " ++ s
