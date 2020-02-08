module Parser.Types
    ( module Parser.Types
    ) where

import qualified Types as T

data Operation = Add
               | Sub
               | Mult
               | Div
               | Exp
               | Mod
               | MatrixMult
               | Other String
               deriving (Show)

data ParseTree = Number Int
               | Float Float
               | Identifier String
               | Complex (T.Complex Float)
               | Oper Operation

               | Array [ParseTree]                  -- [Value]
               | Matrix [ParseTree]                 -- [Array]
               | Funcall (ParseTree, [ParseTree])   -- (String, [Values])
               -- ~ | Expr [ParseTree]
               -- ~ | Assignment ([String], ParseTree)
               -- ~ | Operator Char
               -- ~ | Operation (Char, [ParseTree])
               deriving (Show)
