module Main where

import System.Environment
import SolvePoly

printPolyRes (Left s) = putStrLn $ "ERROR: " ++ s
printPolyRes (Right (a,b,c)) = do
    putStrLn $ "Simplified form: " ++ a
    putStrLn $ "Degree: " ++ show b
    mapM_ putStrLn c

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn "No"
    else
        printPolyRes $ solvePoly $ head args
