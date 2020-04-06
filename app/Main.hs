module Main where

import Data.Functor

import Repl
import ArgumentParser
import Runners

notImplemented str = putStrLn (str ++ " is not implemented")

main :: IO ()
main = getCommand >>= \case
    Run     _   fn -> runFile fn $> ()
    Dump    stg fn -> dumpFileAtStage stg fn
    Compile _   _  -> notImplemented "compiler"
    Repl           -> repl
