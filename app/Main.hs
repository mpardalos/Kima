module Main where

import Kima

notImplemented str = putStrLn (str ++ " is not implemented")

main :: IO ()
main = getCommand >>= \case
    Run     _ fn -> runFile fn
    Compile _ _  -> notImplemented "compiler"
