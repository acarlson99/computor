-- ~ module Parse.Primitives
    -- ~ ( parseNumber
    -- ~ , parseFloat
    -- ~ , parseComplex
    -- ~ , parseIdentifier

    -- ~ , parsePrimitive
    -- ~ , ParseTree (..)
    -- ~ ) where

module Parse.Primitives
    ( module Parse.Primitives
    )
where

import           Data.Functor

import           Parsing

import qualified Types                         as T
import           Parse.Types

parseNumber :: Parser Primitive
parseNumber = Number <$> integer

parseFloat' :: Parser Float
parseFloat' = do
    lhs <- fromIntegral <$> natural
    _   <- char '.'
    rhs <- many digit
    if rhs == "" then return lhs else return $ lhs + read ("0." ++ rhs)

float :: Parser Float
float = (char '-' >> negate <$> parseFloat') <|> parseFloat'

floating :: Parser Float
floating = token float

parseFloat :: Parser Primitive
parseFloat = Float <$> floating

intAsFloat :: Parser Float
intAsFloat = fromIntegral <$> int

comp :: Parser (T.Complex Float)
comp =
    T.complex
        <$> (float <|> intAsFloat)
        <*  char 'i'
        <|> (string "-i" $> T.Complex (0, -1))
        <|> (char 'i' $> T.Complex (0, 1))

complex :: Parser (T.Complex Float)
complex = token comp

parseComplex :: Parser Primitive
parseComplex = Complex <$> token complex

parsePrimitive :: Parser Primitive
parsePrimitive =
    parseComplex
    -- ~ <|> parseIdentifier
        <|> parseFloat
        <|> parseNumber
        <|> do
                _ <- char '('
                x <- parsePrimitive
                _ <- char ')'
                return x
