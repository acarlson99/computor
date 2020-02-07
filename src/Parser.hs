module Parser
    ( readExpr
    , parseNumber
    , parseIdentifier
    , parseFloat
    , ParseTree (..)
    ) where

import Control.Monad

import Parsing

import qualified Types as T

data ParseTree = Expr [ParseTree]
               | Assignment ([String], ParseTree)
               | Identifier String
               | Operator Char
               | Operation (Char, [ParseTree])
               | Number Int
               | Float Float
               | Complex (T.Complex Float)
               deriving (Show)

{-
Operator precedence

-       -- unary minus
()      -- paren/subexpression
^       -- exponent
*/      -- multiplication/division
+-      -- addition/subtraction

=       -- assignment
-}

parseExpr = parseNumber

parseNumber = Number <$> integer

parseIdentifier = Identifier <$> identifier

parseFloat' :: Parser Float
parseFloat' = do
    lhs <- fromIntegral <$> natural
    char '.'
    rhs <- many digit
    if rhs == ""
    then return $ lhs
    else return $ lhs + read ("0." ++ rhs)

floating = do
    char '-'
    num <- parseFloat'
    return $ negate num
    <|> parseFloat'

parseFloat = Float <$> token floating

-- NOTE: not guarenteed to work
-- ~ intAsFloat = do
    -- ~ n <- integer
    -- ~ return $ fromIntegral n

-- ~ parseComplex = do
    -- ~ n <- (floating <|> f)
    -- ~ char 'i'
    -- ~ return $ Complex $ T.Complex (0,n)

readExpr :: String -> String
readExpr = show . parse parseFloat
