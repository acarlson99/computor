module Parser.Types
    ( module Parser.Types
    ) where

import qualified Types as T

data Operator = Add
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
               | Operation (Operator, ParseTree, ParseTree) -- 1 + 2 === + 1 2

               | Array [ParseTree]                  -- [Value]
               | Matrix [ParseTree]                 -- [Array]
               | Funcall (ParseTree, [ParseTree])   -- (String, [Values])
               -- ~ | Expr [ParseTree]
               -- ~ | Assignment ([String], ParseTree)
               -- ~ | Operator Char
               -- ~ | Operation (Char, [ParseTree])
               deriving (Show)
