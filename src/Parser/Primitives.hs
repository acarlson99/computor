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

import Data.Functor

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
    then return lhs
    else return $ lhs + read ("0." ++ rhs)

float = (char '-' >> negate <$> parseFloat')
    <|> parseFloat'

floating = token float

parseFloat = Float <$> floating

intAsFloat = fromIntegral <$> int

-- ~ comp = do
-- ~ comp = T.complex <$> (float <|> intAsFloat) <* char 'i'
    -- ~ <|> do
        -- ~ string "-i"
        -- ~ return $ T.Complex (0,-1)
    -- ~ <|> do
        -- ~ char 'i'
        -- ~ return $ T.Complex (0,1)
comp = T.complex <$> (float <|> intAsFloat) <* char 'i'
    <|> (string "-i" $> T.Complex (0,-1))
    <|> (   char 'i' $> T.Complex (0, 1))

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
