module Main where

import Kima
import Control.Newtype.Generics

notImplemented str = putStrLn (str ++ " is not implemented")

runInterpreter :: RunOpts -> FilePath -> IO ()
runInterpreter RunOpts path = parseProgram path <$> readFile path >>= \case
    Left  err -> putStrLn (parseErrorPretty err)
    Right ast -> case runTypeChecking baseCtx (checkProgram ast) of
        Left  err -> print err
        Right _   -> runProgram (over Program (fmap @[] desugarFuncDef) ast) >>= \case
            Left  _   -> putStrLn "Runtime Error"
            Right _   -> return ()

main :: IO ()
main = getCommand >>= \case
    Run     opts fn -> runInterpreter opts fn
    Compile _    _  -> notImplemented "compiler"
