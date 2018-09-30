module Main where

import Control.Monad.State
import Text.Megaparsec

import qualified Typechecking as T
import qualified Frontend as F

evalChecker :: (a -> T.KTypeM T.KType) -> a -> Either T.TypeError T.KType
evalChecker f = (`evalStateT` mempty) . T.runKTypeM . f

evalCheckExpr  = evalChecker T.checkExpr
evalCheckStmt  = evalChecker T.checkStmt
evalCheckBlock = evalChecker T.checkBlock

parseBlock = either (putStrLn . parseErrorPretty) print . runParser F.block ""
parseStmt = either (putStrLn . parseErrorPretty) print . runParser F.stmt ""
parseExpr = either (putStrLn . parseErrorPretty) print . runParser F.expr ""

main :: IO ()
main = putStrLn "hello world"
