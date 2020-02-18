module Interpreter
    ( interpret
    )
where

import           System.Console.Readline

import qualified Poly.Solve                    as P

import           Parse
import           Eval

helpMsg :: String
helpMsg =
    "Commands:\n\
    \\t@help    help msg\n\
    \\t@quit    quit\n\
    \\t@dump    show all defined variables/functions\n\
    \\t@reset   clear all definitions\n\
    \\t@poly    evaluate polynomial\n\
    \Data types:\n\
    \\tInt      5\n\
    \\tInt      -20\n\
    \\tFloat    3.5\n\
    \\tComplex  6.2i\n\
    \\tMatrix   [[1, 2]; [3, 4]]\n\
    \\tMatrix   [1.3, 2i]; [3 + 2i, -4.3 - 2.2i]]\n\
    \Operations:\n\
    \\t^        exponent\n\
    \\t**       matrix multiplication\n\
    \\t*        multiplication\n\
    \\t/        division\n\
    \\t%        mod (integers only)\n\
    \\t+        addition\n\
    \\t-        subtraction\n"

-- run builtin commands
evalCmd :: (Show t, Num t) => Cmd -> CalcState -> t -> IO ()
evalCmd Quit _  _  = return ()
evalCmd Help st ln = do
    putStrLn helpMsg
    interpretLn st ln
evalCmd (EvalPoly xs) st ln = do
    P.printRes $ P.solve xs
    interpretLn st ln
evalCmd Reset _  ln = interpretLn emptyState ln
evalCmd Dump  st ln = do
    print st
    interpretLn st ln

-- evaluate parsed expression && call interpret on new state
evalExpr :: (Show t, Num t) => [(ParseTree, String)] -> CalcState -> t -> IO ()
evalExpr [(Command cmd, "")] st ln = evalCmd cmd st ln
evalExpr expr state lnum =
    let res = eval expr state
    in  case res of
            Right (newSt, io) -> do
                io
                interpretLn newSt lnum
            Left err -> do
                putStrLn $ "ERROR: " ++ err
                interpretLn state lnum

-- read line, parse, evaluate, recurse
interpretLn :: (Show t, Num t) => CalcState -> t -> IO ()
interpretLn state linenum = do
    maybeLine <- readline $ show linenum ++ "# "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do
            addHistory ln
            evalExpr (readExpr ln) state (linenum + 1)

interpret :: [String] -> IO ()
interpret ("help"     : _ ) = putStrLn helpMsg
interpret _                 = interpretLn emptyState (0 :: Integer)
