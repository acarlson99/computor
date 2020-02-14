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
strToOperator "*"  = Mult
strToOperator "/"  = Div
strToOperator "^"  = Exp
strToOperator "**" = MatrixMult
strToOperator s    = Other s

parseOperator :: Parser Operator
parseOperator = strToOperator <$> token operator
