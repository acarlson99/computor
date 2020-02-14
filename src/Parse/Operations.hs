module Parse.Operations
    ( parseOperator
    , strToOperator
    )
where

import           Parsing

import           Parse.Types

oper =
    string "+"
        <|> string "-"
        <|> string "**"
        <|> string "*"
        <|> string "/"
        <|> string "^"

operator = token oper

strToOperator "+"  = Add
strToOperator "-"  = Sub
strToOperator "**" = MatrixMult
strToOperator "*"  = Mult
strToOperator "/"  = Div
strToOperator "^"  = Exp
strToOperator s    = Other s

parseOperator :: Parser Operator
parseOperator = strToOperator <$> token operator
