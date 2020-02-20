module Interpreter
    ( interpret
    )
where

import           System.Console.Readline
import           System.IO
import qualified System.IO.Strict              as S
import           Control.Exception

import qualified Poly.Solve                    as P

import           Parse
import           Eval
import           State

helpMsg :: String
helpMsg =
    "Commands:\n\
    \\t@help    help msg\n\
    \\t@quit    quit\n\
    \\t@dump    show all defined variables/functions\n\
    \\t@reset   clear all definitions\n\
    \\t@poly    evaluate polynomial\n\
    \\nData types:\n\
    \\tInt      5\n\
    \\tInt      -20\n\
    \\tFloat    3.5\n\
    \\tComplex  6.2i\n\
    \\tMatrix   [[1, 2]; [3, 4]]\n\
    \\tMatrix   [1.3, 2i]; [3 + 2i, -4.3 - 2.2i]]\n\
    \\nOperations:\n\
    \\t^        exponent\n\
    \\t**       matrix multiplication\n\
    \\t*        multiplication\n\
    \\t/        division\n\
    \\t%        mod (integers only)\n\
    \\t+        addition\n\
    \\t-        subtraction\n\
    \\nAssignment:\n\
    \\ta           = 42\n\
    \\tf(a)        = a * 2\n\
    \\tfunc(a,b,c) = a * b ^ c"

-- run builtin commands
evalCmd :: (Show t, Num t) => Cmd -> State -> t -> IO ()
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
evalExpr :: (Show t, Num t) => [(ParseTree, String)] -> State -> t -> IO ()
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
interpretLn :: (Show t, Num t) => State -> t -> IO ()
interpretLn state linenum = do
    maybeLine <- readline $ show linenum ++ "# "
    case maybeLine of
        Nothing -> return ()
        Just ln -> do
            addHistory ln
            evalExpr (readExpr ln) state (linenum + 1)

presets :: [String]
presets =
    [ "pi = 3.1415926535"
    , "e = 2.71828"
    , "identity2 = [[1,0];[0,1]]"
    , "identity3 = [[1,0,0];[0,1,0];[0,0,1]]"
    , "identity4 = [[1,0,0,0];[0,1,0,0];[0,0,1,0];[0,0,0,1]]"
    , "sqrt(x) = x ^ (1./2.)"
    , "float(x) = x * 1."
    ]

-- file IO

interpretContents :: State -> String -> Handle -> IO (Either String State)
interpretContents st filename hnd = do
    content <- lines <$> S.hGetContents hnd
    case evalArr (map readExpr content) st of
        Right newSt -> return $ Right newSt
        Left  err   -> return $ Left $ filename ++ ": " ++ err

handleIOException :: IOException -> IO (Either String a)
handleIOException err = return $ Left $ show err

interpretFile :: String -> State -> IO (Either String State)
interpretFile x st =
    catch (withFile x ReadMode (interpretContents st x)) handleIOException

loadFiles :: [String] -> State -> IO (Either String State)
loadFiles []       st = return $ Right st
loadFiles (x : xs) st = do
    nst <- interpretFile x st
    case nst of
        Right newSt -> loadFiles xs newSt
        Left  err -> return $ Left err

loadAndRun :: [String] -> State -> IO ()
loadAndRun xs st = do
    nst <- loadFiles xs st
    case nst of
        Right newSt -> interpretLn newSt (0 :: Integer)
        Left  err   -> putStrLn $ "Error loading file " ++ err

interpret :: [String] -> IO ()
interpret ("help" : _     ) = putStrLn helpMsg
interpret ["nopreset"     ] = interpretLn emptyState (0 :: Integer)
interpret ("nopreset" : xs) = loadAndRun xs emptyState
interpret xs                = case evalArr (map readExpr presets) emptyState of
    Right st  -> loadAndRun xs st
    Left  err -> putStrLn $ "ERROR LOADING PRESETS: " ++ err
