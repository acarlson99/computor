module Main where

import           System.Environment

import           Poly.Solve
import           Interpreter
import           Eval

printUsage :: IO ()
printUsage = do
    putStrLn "usage: stack run (help|poly|repl) [args]"
    putStrLn "       poly - evaluate polynomial"
    putStrLn "              stack run poly \"3X^2 + 2X = -4\""
    putStrLn "       repl - run repl.  NOTE: not yet implemented"
    putStrLn "       help - help message"

runComp :: [[Char]] -> IO ()
runComp ("help"     : _) = printUsage
runComp ("poly" : x : xs) = mapM_ (printRes . solve) $ x : xs
-- runComp ("repl"     : _) = interpret emptyState 0
runComp ("repl"     : _) = interpret emptyState ((0) :: Integer)
runComp _                 = printUsage

main :: IO ()
main = do
    args <- getArgs
    runComp args
