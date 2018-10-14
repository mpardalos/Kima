module Main where

import           ArgumentParser
import           Frontend
import           Typechecking
import           Interpreter

notImplemented str = putStrLn (str ++ " is not implemented")

runInterpreter :: RunOpts -> FilePath -> IO ()
runInterpreter RunOpts path = parseProgram path <$> readFile path >>= \case
    Left  err -> putStrLn (parseErrorPretty err)
    Right ast -> do
        case runTypeChecking (checkProgram ast) of
            Left  err -> print err
            Right _   -> pure ()
        execInterpreter (runProgram ast) >>= \case
            Left  _   -> putStrLn "Runtime Error"
            Right _   -> return ()

main :: IO ()
main = getCommand >>= \case
    Run     opts fn -> runInterpreter opts fn
    Compile _    _  -> notImplemented "compiler"
