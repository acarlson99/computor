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
import           Parse.Operations
import           Parse.Types

parseNumber = Number <$> integer

parseFloat' :: Parser Float
parseFloat' = do
    lhs <- fromIntegral <$> natural
    char '.'
    rhs <- many digit
    if rhs == "" then return lhs else return $ lhs + read ("0." ++ rhs)

float = (char '-' >> negate <$> parseFloat') <|> parseFloat'

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
comp =
    T.complex
        <$> (float <|> intAsFloat)
        <*  char 'i'
        <|> (string "-i" $> T.Complex (0, -1))
        <|> (char 'i' $> T.Complex (0, 1))

complex = token comp

parseComplex = Complex <$> token complex

parsePrimitive :: Parser Primitive
parsePrimitive =
    parseComplex
    -- ~ <|> parseIdentifier
        <|> parseFloat
        <|> parseNumber
        <|> do
                char '('
                x <- parsePrimitive
                char ')'
                return x
