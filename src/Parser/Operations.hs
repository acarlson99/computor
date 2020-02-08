module Parser.Operations
    ( Operation (..)
    , parseOperation
    ) where

import Parsing

import Parser.Types

oper = string "+"
   <|> string "-"
   <|> string "**"
   <|> string "*"
   <|> string "/"
   <|> string "^"

operation = token oper

strToOperation "+"  = Add
strToOperation "-"  = Sub
strToOperation "*"  = Mult
strToOperation "/"  = Div
strToOperation "^"  = Exp
strToOperation "**" = MatrixMult
strToOperation s    = Other s

parseOperation :: Parser ParseTree
parseOperation = Oper <$> strToOperation <$> operation
