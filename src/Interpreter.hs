module Interpreter
    ( interpret
    )
where

import           System.Console.Readline

import qualified Poly.Solve                    as P

import           Parse
import           Eval

-- run builtin commands
evalCmd Quit st ln = return ()
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

evalExpr [(Command cmd, "")] st ln = evalCmd cmd st ln
evalExpr exp state lnum =
    -- ~ let (newst, pm) = eval exp state
    let res = eval exp state
    in case res of
        Right (newSt,io) -> do io
                               interpret newSt lnum
        Left  err        -> do putStrLn $ "ERROR: " ++ err
                               interpret state lnum

-- read line, parse, evaluate, recurse
interpret state linenum = do
    maybeLine <- readline $ show linenum ++ " > "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do
            addHistory ln
            evalExpr (readExpr ln) state (linenum + 1)
