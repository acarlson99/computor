module Interpreter
    ( interpret
    ) where

import System.IO

import Parser

interpret :: Int -> [String] -> IO ()
interpret linenum _ = do
    putStr $ show linenum ++ " > "
    hFlush stdout
    ineof <- isEOF
    if ineof
    then return ()
    else do ln <- getLine
            putStrLn $ readExpr ln
            interpret (linenum + 1) []
