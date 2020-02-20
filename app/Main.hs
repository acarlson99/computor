module Main where

import           System.Environment

import           Poly.Solve
import           Interpreter

printUsage :: IO ()
printUsage = do
    putStrLn "usage: stack run -- --(help|poly|repl) [args]"
    putStrLn "       --poly - evaluate polynomial"
    putStrLn "                stack run poly \"3X^2 + 2X = -4\""
    putStrLn "       --repl - run repl"
    putStrLn "       --help - help message"

runComp :: [String] -> IO ()
runComp ("--help"     : _ ) = printUsage
runComp ("--poly" : x : xs) = mapM_ (printRes . solve) $ x : xs
runComp ("--repl"     : xs) = interpret xs
runComp _                   = printUsage

main :: IO ()
main = do
    args <- getArgs
    runComp args
