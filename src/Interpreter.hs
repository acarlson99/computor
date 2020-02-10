module Interpreter
    ( interpret
    , emptyState
    ) where

import System.Console.Readline

import qualified Poly.Solve as P

import Parser.Parser
import Parser.Types

newtype CalcState = C [String]

emptyState :: CalcState
emptyState = C []

walkParseTree :: CalcState -> [(ParseTree,String)] -> (CalcState, IO ())
-- commands
walkParseTree st [(Command Help,[])] = (st, putStrLn "HELP MSG")
walkParseTree st [(Command (EvalPoly pn),[])] = (st, P.printRes $ P.solve pn)
walkParseTree st [(Command Quit,[])] = (st, return ())
walkParseTree st [(Command Reset,[])] = (emptyState, return ())
-- catch-all
walkParseTree st [(expr,[])] = (st, putStrLn $ "UNIMPLEMENTED: " ++ show expr)
-- error cases
walkParseTree st [] = (st, return ())
walkParseTree st (x:y:ys) = (st,
    putStrLn "WAIT HOLD UP THE HELL HAPPENED THIS SHOULDNT HAPPEN WTFFFFF")
walkParseTree st [(expr,s)]  = (st,
    putStrLn $ "Unexpected trailing tokens: '" ++ s ++ "' in expression '" ++ show expr ++ "'")

-- ~ interpret :: Int -> [String] -> IO ()
interpret linenum _ = do
    maybeLine <- readline $ show linenum ++ " > "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do addHistory ln
                      let expr = readExpr ln
                      case expr of
                        (Command Quit,""):xs -> return ()
                        _                    -> let (newState,b) = walkParseTree (C []) expr
                                                in do
                                                b
                                                interpret (linenum + 1) newState
