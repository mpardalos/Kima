module Main where

import           System.Environment
import           Test.Hspec.Runner

import           Kima.Test.XmlFormatter
import           Kima.Test.FileTests           as FileTests
import           Kima.Test.Parser              as Parser

main :: IO ()
main = do
    args <- getArgs
    let config =
            defaultConfig { configFormatter = if "--junit-output" `elem` args then Just xmlFormatter else Nothing }
    withArgs (filter (/= "--junit-output") args) $ hspecWith config $ do
        Parser.spec
        FileTests.spec
