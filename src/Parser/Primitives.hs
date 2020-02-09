-- ~ module Parser.Primitives
    -- ~ ( parseNumber
    -- ~ , parseFloat
    -- ~ , parseComplex
    -- ~ , parseIdentifier

    -- ~ , parsePrimitive
    -- ~ , ParseTree (..)
    -- ~ ) where

module Parser.Primitives
    ( module Parser.Primitives
    ) where

import Parsing

import qualified Types as T
import Parser.Operations
import Parser.Types

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

parseFloat = Float <$> floating

intAsFloat = do
    n <- int
    return $ fromIntegral n

comp = T.complex <$> (float <|> intAsFloat) <* char 'i'
    <|> do
        token $ string "-i"
        return $ T.Complex (0,-1)
    <|> do
        char 'i'
        return $ T.Complex (0,1)

complex = token comp

parseComplex = Complex <$> token complex

parsePrimitive =
        parseComplex
    <|> parseIdentifier
    <|> parseFloat
    <|> parseNumber
    <|> do
        char '('
        x <- parsePrimitive
        char ')'
        return x