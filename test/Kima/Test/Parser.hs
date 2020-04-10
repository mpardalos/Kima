{-# LANGUAGE OverloadedStrings #-}
module Kima.Test.Parser (spec) where

import Control.Monad
import Kima.AST
import Kima.Syntax.Parser
import Test.Hspec
import Text.Megaparsec

import GHC.Exts

spec :: Spec
spec = parallel $ describe "Parser" $ do
    describe "Term parser" $
        forM_ termTests $ \(str, ast) ->
            it ("Parses " <> str <> " correctly") $
                str `parsedBy` term `shouldParseTo` ast

    describe "Expression Parser" $
        forM_ expressionTests $ \(str, ast) ->
            it ("Parses " <> str <> " correctly") $
                str `parsedBy` expr `shouldParseTo` ast

    describe "Statement Parser" $
        forM_ statementTests $ \(str, ast) ->
            it ("Parses " <> str <> " correctly") $
                str `parsedBy` stmt `shouldParseTo` ast

    describe "Type Parser" $
        forM_ typeTests $ \(str, t) ->
            it ("Parses " <> str <> " correctly") $
                str `parsedBy` typeExpr `shouldParseTo` t

termTests :: [(String, Expr Parsed)]
termTests =
    [ ("name"   , IdentifierExpr "name")
    , ("1.23"   , LiteralExpr    (FloatLit 1.23))
    , ("5"      , LiteralExpr    (IntLit 5))
    , ("\"hi\"" , LiteralExpr    (StringLit "hi"))
    , ("\"123\"", LiteralExpr    (StringLit "123"))
    , ("False"  , LiteralExpr    (BoolLit False))
    , ("True"   , LiteralExpr    (BoolLit True))
    , ("()"     , LiteralExpr    UnitLit)
    ]

expressionTests :: [(String, Expr Parsed)]
expressionTests =
    [ ("5 + 5", BinExpr AddOp (LiteralExpr (IntLit 5)) (LiteralExpr (IntLit 5)))
    , ("5 * 5", BinExpr MulOp (LiteralExpr (IntLit 5)) (LiteralExpr (IntLit 5)))
    , ("5 - 5", BinExpr SubOp (LiteralExpr (IntLit 5)) (LiteralExpr (IntLit 5)))
    , ("5 / 5", BinExpr DivOp (LiteralExpr (IntLit 5)) (LiteralExpr (IntLit 5)))
    , ("5 % 5", BinExpr ModOp (LiteralExpr (IntLit 5)) (LiteralExpr (IntLit 5)))
    , ("-5"   , UnaryExpr NegateOp (LiteralExpr (IntLit 5)))
    , ( "\"a\" + \"b\""
      , BinExpr AddOp (LiteralExpr (StringLit "a")) (LiteralExpr (StringLit "b"))
      )
    , ("10(5)", CallExpr (LiteralExpr (IntLit 10)) [LiteralExpr (IntLit 5)])
    , ("func(5)", CallExpr (IdentifierExpr "func") [LiteralExpr (IntLit 5)])
    , ("func(5)(\"hi\")"
      , CallExpr
            (CallExpr (IdentifierExpr "func") [LiteralExpr (IntLit 5)])
            [LiteralExpr (StringLit "hi")]
      )
    , ( "a.b",     IdentifierExpr "a" `AccessExpr` "b")
    , ( "a().b",   CallExpr (IdentifierExpr "a") [] `AccessExpr` "b")
    , ( "(1+4).b", BinExpr AddOp (LiteralExpr (IntLit 1)) (LiteralExpr (IntLit 4)) `AccessExpr` "b")
    , ( "a.b.c",   (IdentifierExpr "a" `AccessExpr` "b") `AccessExpr` "c")
    , ( "fun () -> Unit {}", FuncExpr [] Nothing "Unit" (BlockStmt []))
    , ( "fun (a: Int) -> Unit {}", FuncExpr [("a", "Int")] Nothing "Unit" (BlockStmt []))
    , ( "fun (a: Int, b: Int) -> Unit {}", FuncExpr [("a", "Int"), ("b", "Int")] Nothing "Unit" (BlockStmt []))
    , ( "fun () : eff -> Unit {}", FuncExpr [] (Just $ fromList ["eff"]) "Unit" (BlockStmt []))
    , ( "fun (a: Int) : eff -> Unit {}", FuncExpr [("a", "Int")] (Just $ fromList ["eff"]) "Unit" (BlockStmt []))
    , ( "fun (a: Int, b: Int) : eff -> Unit {}", FuncExpr [("a", "Int"), ("b", "Int")] (Just $ fromList ["eff"]) "Unit" (BlockStmt []))
    ]

statementTests :: [(String, Stmt Parsed)]
statementTests =
    [ ("while True { print(name); }", WhileStmt (
        While (LiteralExpr $ BoolLit True) $ BlockStmt [
            ExprStmt $ CallExpr (IdentifierExpr "print") [IdentifierExpr "name"]
        ]))
    , ("if True { print(name1); } else { print(name2); }", IfStmt (
        If (LiteralExpr $ BoolLit True) (BlockStmt [
            ExprStmt $ CallExpr (IdentifierExpr "print") [IdentifierExpr "name1"]
        ]) $ BlockStmt [
            ExprStmt $ CallExpr (IdentifierExpr "print") [IdentifierExpr "name2"]
        ]
        ))
    , ("if True { print(name1); } ", SimpleIfStmt
          (LiteralExpr $ BoolLit True)
          (BlockStmt [ExprStmt $ CallExpr (IdentifierExpr "print") [IdentifierExpr "name1"]]))
    ]

typeTests :: [(String, ParsedTypeExpr)]
typeTests =
    [ ( "a"
      , ParsedTypeName "a"
      )
    -- Simple functions
    , ( "(a) -> b"
      , ParsedSignatureType [ParsedTypeName "a"] Nothing (ParsedTypeName "b")
      )
    , ( "(a, b) -> c"
      , ParsedSignatureType [ParsedTypeName "a", ParsedTypeName "b"]
                            Nothing
                            (ParsedTypeName "c")
      )
    -- Effects
    , ( "(a) : eff -> c"
      , ParsedSignatureType [ParsedTypeName "a"]
                            (Just $ fromList ["eff"])
                            (ParsedTypeName "c")
      )
    , ( "(a) : {eff1, eff2} -> c"
      , ParsedSignatureType [ParsedTypeName "a"]
                            (Just $ fromList ["eff1", "eff2"])
                            (ParsedTypeName "c")
      )
    -- Nested functions
    , ( "(a) -> (b) -> c"
      , ParsedSignatureType
          [ParsedTypeName "a"]
          Nothing
          (ParsedSignatureType [ParsedTypeName "b"] Nothing (ParsedTypeName "c")
          )
      )
    , ( "(a) : eff -> (b) -> c"
      , ParsedSignatureType
          [ParsedTypeName "a"]
          (Just $ fromList ["eff"])
          (ParsedSignatureType [ParsedTypeName "b"] Nothing (ParsedTypeName "c")
          )
      )
    , ( "(a) -> (b) : eff -> c"
      , ParsedSignatureType
          [ParsedTypeName "a"]
          Nothing
          (ParsedSignatureType [ParsedTypeName "b"]
                               (Just $ fromList ["eff"])
                               (ParsedTypeName "c")
          )
      )
    , ( "(a) : eff1 -> (b) : eff2 -> c"
      , ParsedSignatureType
          [ParsedTypeName "a"]
          (Just $ fromList ["eff1"])
          (ParsedSignatureType [ParsedTypeName "b"]
                               (Just $ fromList ["eff2"])
                               (ParsedTypeName "c")
          )
      )
    ]

type ParseTestResult e s a = Either (ParseErrorBundle s e) (s, a)

parsedBy :: s -> Parsec e s a -> ParseTestResult e s a
parsedBy str p = case runParser' p initialState of
    (_, Left err ) -> Left err
    (s, Right res) -> Right (stateInput s, res)
  where
    initialState = State
        { stateInput    = str
        , stateOffset   = 0
        , statePosState = PosState { pstateInput      = str
                                   , pstateOffset     = 0
                                   , pstateSourcePos  = initialPos ""
                                   , pstateTabWidth   = defaultTabWidth
                                   , pstateLinePrefix = ""
                                   }
        }

shouldParseTo
    :: (Eq a, Show a, Show s, Show e, Stream s, ShowErrorComponent e)
    => ParseTestResult e s a
    -> a
    -> Expectation
shouldParseTo (Right (_, actual)) expected = actual `shouldBe` expected
shouldParseTo (Left err) _ =
    expectationFailure ("Got error " <> errorBundlePretty err)
