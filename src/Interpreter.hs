module Interpreter
    ( interpret
    ) where

import System.IO

import Parser

interpret :: [String] -> IO ()
interpret _ = do
    putStr "COMP> "
    hFlush stdout
    ineof <- isEOF
    if ineof
    then return ()
    else do ln <- getLine
            putStrLn $ readExpr ln
            interpret []
