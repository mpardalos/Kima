module Main where

import AST
import ArgumentParser
import Data.Newtype
import Frontend
import Interpreter
import Typechecking

notImplemented str = putStrLn (str ++ " is not implemented")

runInterpreter :: RunOpts -> FilePath -> IO ()
runInterpreter RunOpts path = parseProgram path <$> readFile path >>= \case
    Left  err -> putStrLn (parseErrorPretty err)
    Right ast -> do
        case runTypeChecking mempty (checkProgram ast) of
            Left  err -> print err
            Right _   -> pure ()
        runProgram (fmap @[] desugarFuncDef `under` ast) >>= \case
            Left  _   -> putStrLn "Runtime Error"
            Right _   -> return ()

main :: IO ()
main = getCommand >>= \case
    Run     opts fn -> runInterpreter opts fn
    Compile _    _  -> notImplemented "compiler"
