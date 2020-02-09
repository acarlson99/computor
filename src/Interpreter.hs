module Interpreter
    ( interpret
    ) where

import System.Console.Readline

import Parser.Parser

interpret :: Int -> [String] -> IO ()
interpret linenum _ = do
    maybeLine <- readline $ show linenum ++ " > "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do addHistory ln
                      putStrLn $ readExpr ln
                      interpret (linenum + 1) []
