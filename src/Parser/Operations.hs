module Parser.Operations
    ( Operation (..)
    , parseOperation
    ) where

import Parser.Types
import Parser.Parsing

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
