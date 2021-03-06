module Parse.Primitives
    ( parseNumber
    , parseFloat
    , parseComplex
    , parsePrimitive
    , ParseTree(..)
    )
where

import           Parsing

import qualified Complex                       as C
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

comp :: Parser (C.Complex Float)
comp = C.complex <$> (float <|> intAsFloat) <* char 'i'
        -- <|> (string "-i" $> C.Complex (0, -1))
        -- <|> (char 'i' $> C.Complex (0, 1))

complex :: Parser (C.Complex Float)
complex = token comp

parseComplex :: Parser Primitive
parseComplex = Complex <$> token complex

parsePrimitive :: Parser Primitive
parsePrimitive = parseComplex <|> parseFloat <|> parseNumber <|> do
    _ <- char '('
    x <- parsePrimitive
    _ <- char ')'
    return x
