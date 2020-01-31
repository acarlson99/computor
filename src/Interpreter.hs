module Interpreter
    ( interpret
    ) where

import System.IO

interpret :: [String] -> IO ()
interpret _ = do
    putStr "> "
    hFlush stdout
    ineof <- isEOF
    if ineof
    then return ()
    else do ln <- getLine
            putStrLn ln
            interpret []
