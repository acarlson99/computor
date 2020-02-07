module Parser.Primitive
    ( parseNumber
    , parseFloat
    , parseComplex
    , parseIdentifier

    , parsePrimitive
    , ParseTree (..)
    ) where

import qualified Types as T
import Parser.Parsing

data ParseTree = Number Int
               | Identifier String
               | Float Float
               | Complex (T.Complex Float)
               | Array [ParseTree]
               | Matrix [ParseTree]
               -- ~ | Expr [ParseTree]
               -- ~ | Assignment ([String], ParseTree)
               -- ~ | Operator Char
               -- ~ | Operation (Char, [ParseTree])
               deriving (Show)

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

float = do
    char '-'
    num <- parseFloat'
    return $ negate num
    <|> parseFloat'

floating = token float

parseFloat = Float <$> token floating

intAsFloat = do
    n <- integer
    return $ fromIntegral n

comp = do
    n <- (floating <|> intAsFloat)
    char 'i'
    return $ T.Complex (0,n)

complex = token comp

parseComplex = do
    n <- (floating <|> intAsFloat)
    char 'i'
    return $ Complex $ T.Complex (0,n)
    <|> do
        char 'i'
        return $ Complex $ T.Complex (0,1)

parsePrimitive =
        parseComplex
    <|> parseIdentifier
    <|> parseFloat
    <|> parseNumber
