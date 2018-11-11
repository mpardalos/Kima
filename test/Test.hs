module Main where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec.Megaparsec

import qualified Text.Megaparsec as MP
import Kima.Frontend.Tokenizer
import Kima.Frontend

parse p = MP.parse p ""

main :: IO ()
main = defaultMain =<< testSpec "Parser" parserSpec

parserSpec :: Spec
parserSpec = describe "Tokenizer" $ do  
    it "parses ints" $
        parse intLiteral "10" `shouldParse` 10

    it "parses strings" $
        parse string "\"abcd\"" `shouldParse` "abcd"

    it "does not parse reserved words as identifiers" $
        parse identifier `shouldFailOn` "while"