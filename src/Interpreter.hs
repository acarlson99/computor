module Interpreter
    ( interpret
    )
where

import           System.Console.Readline

import qualified Poly.Solve                    as P

import           Parse
import           Eval

-- run builtin commands
evalCmd :: (Show t, Num t) => Cmd -> CalcState -> t -> IO ()
evalCmd Quit _ _ = return ()
evalCmd Help st ln = do
    print "HELP MSG"
    interpret st ln
evalCmd (EvalPoly xs) st ln = do
    P.printRes $ P.solve xs
    interpret st ln
evalCmd Reset _  ln = interpret emptyState ln
evalCmd Dump  st ln = do
    print st
    interpret st ln

evalExpr :: (Show t, Num t) => [(ParseTree, String)] -> CalcState -> t -> IO ()
evalExpr [(Command cmd, "")] st ln = evalCmd cmd st ln
evalExpr expr state lnum =
    -- ~ let (newst, pm) = eval exp state
    let res = eval expr state
    in  case res of
            Right (newSt, io) -> do
                io
                interpret newSt lnum
            Left err -> do
                putStrLn $ "ERROR: " ++ err
                interpret state lnum

-- read line, parse, evaluate, recurse
interpret :: (Show t, Num t) => CalcState -> t -> IO ()
interpret state linenum = do
    maybeLine <- readline $ show linenum ++ "# "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do
            addHistory ln
            -- ~ evalExpr (readExpr ln) state (linenum + 1)
            let expr = readExpr ln
            putStrLn $ "Parsed: " ++ show expr
            evalExpr expr state (linenum + 1)
