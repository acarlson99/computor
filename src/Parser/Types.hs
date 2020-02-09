module Parser.Types
    ( module Parser.Types
    ) where

import qualified Types as T
import Util

data Operator = Add
               | Sub
               | Mult
               | Div
               | Exp
               | Mod
               | MatrixMult
               | Other String

data ParseTree = Number Int
               | Float Float
               | Identifier String
               | Complex (T.Complex Float)
               | Operation (Operator, ParseTree, ParseTree) -- 1 + 2 === + 1 2

               | Array [ParseTree]                  -- [Value]
               | Matrix [ParseTree]                 -- [Array]
               | Funcall (ParseTree, [ParseTree])   -- (String, [Values])

               | Assignment (ParseTree, ParseTree)
               | Defun (ParseTree, ParseTree) -- (funcall, expr)
               -- ~ | Expr [ParseTree]
               -- ~ | Assignment ([String], ParseTree)
               -- ~ | Operator Char
               -- ~ | Operation (Char, [ParseTree])
               -- ~ deriving (Show)

instance Show Operator where
    show Add        = "+"
    show Sub        = "-"
    show Mult       = "*"
    show Div        = "/"
    show Exp        = "^"
    show MatrixMult = "**"
    show (Other s)  = s

instance Show ParseTree where
    show (Number n) = show n
    show (Float n) = show n
    show (Identifier s) = s
    show (Complex c) = show c
    show (Operation (op,lhs,rhs)) = '(' : show lhs ++ ' ' : show op ++ ' ' : show rhs ++ ")"

    show (Array (x:xs)) = '[' : show x  ++ showSepList ',' xs ++ "]"
    show (Array []) = "[ ]"
    show (Matrix (x:xs)) = '[' : show x ++ showSepList ';' xs ++ "]"
    show (Matrix []) = "[ ]"
    show (Funcall (f,x:xs)) = show f ++ '(' : show x ++ showSepList ',' xs ++ ")"
    show (Funcall (f,[])) = show f ++ "( )"

    show (Assignment (f,xs)) = show f ++ " = " ++ show xs
    show (Defun (f,xs)) = show f ++ " = " ++ show xs
