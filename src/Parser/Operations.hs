module Parser.Operations
    ( parseOperator
    ) where

import Parsing

import Parser.Types

oper = string "+"
   <|> string "-"
   <|> string "**"
   <|> string "*"
   <|> string "/"
   <|> string "^"

operator = token oper

strToOperator "+"  = Add
strToOperator "-"  = Sub
strToOperator "*"  = Mult
strToOperator "/"  = Div
strToOperator "^"  = Exp
strToOperator "**" = MatrixMult
strToOperator s    = Other s

parseOperator :: Parser Operator
parseOperator = strToOperator <$> token operator