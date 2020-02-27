module Parse.Operand
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
        <|> string "%"

operator :: Parser String
operator = token oper

strToOperator :: String -> Operator
strToOperator "+"  = Add
strToOperator "-"  = Sub
strToOperator "**" = MatrixMult
strToOperator "*"  = Mult
strToOperator "/"  = Div
strToOperator "^"  = Exp
strToOperator "%"  = Mod
-- comparators
strToOperator "<"  = Lt
strToOperator ">"  = Gt
strToOperator "==" = Eq
strToOperator "<=" = Leq
strToOperator ">=" = Geq
-- logical operations
strToOperator "||" = Or
strToOperator "&&" = And
-- err
strToOperator s    = Other s

parseOperator :: Parser Operator
parseOperator = strToOperator <$> token operator
