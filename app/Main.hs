module Main where

import System.Environment

import Poly.Solve
import Interpreter

printUsage = do
    putStrLn "usage: stack run (help|poly|repl) [args]"
    putStrLn "       poly - evaluate polynomial"
    putStrLn "              stack run poly \"3X^2 + 2X = -4\""
    putStrLn "       repl - run repl.  NOTE: not yet implemented"
    putStrLn "       help - help message"

runComp ("help":xs) = printUsage
runComp ("poly":x:xs) = mapM_ (printRes . solve) $ x:xs
runComp ("repl":xs) = interpret 0 emptyState
runComp _ = printUsage

main :: IO ()
main = do
    args <- getArgs
    runComp args
