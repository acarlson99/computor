module Parse.Operations
    ( parseOperator
    , strToOperator
    )
where

import           Parsing

import           Parse.Types

oper :: Parser String
oper =
    string "+"
        <|> string "-"
        <|> string "**"
        <|> string "*"
        <|> string "/"
        <|> string "^"
  -- TODO: add modulo

operator :: Parser String
operator = token oper

strToOperator :: String -> Operator
strToOperator "+"  = Add
strToOperator "-"  = Sub
strToOperator "**" = MatrixMult
strToOperator "*"  = Mult
strToOperator "/"  = Div
strToOperator "^"  = Exp
strToOperator s    = Other s

parseOperator :: Parser Operator
parseOperator = strToOperator <$> token operator
