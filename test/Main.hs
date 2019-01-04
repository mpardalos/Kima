module Main where

import           System.Environment
import           Test.Hspec.Runner

import           XmlFormatter
import           FileTests

main :: IO ()
main = do
        args <- getArgs
        let config = defaultConfig
                    { configFormatter = if "--junit-output" `elem` args
                                                then Just xmlFormatter
                                                else Nothing
                    }
        withArgs (filter (/= "--junit-output") args) $ hspecWith config fileTestSpec
