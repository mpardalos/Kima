{-# LANGUAGE OverloadedStrings #-}
module Kima.Test.Parser  where

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

termTests :: [(String, AST 'Expr Parsed)]
termTests =
    [ ("name"   , IdentifierE "name")
    , ("1.23"   , LiteralE    (FloatExpr 1.23))
    , ("5"      , LiteralE    (IntExpr 5))
    , ("\"hi\"" , LiteralE    (StringExpr "hi"))
    , ("\"123\"", LiteralE    (StringExpr "123"))
    , ("False"  , LiteralE    (BoolExpr False))
    , ("True"   , LiteralE    (BoolExpr True))
    ]

expressionTests :: [(String, AST 'Expr Parsed)]
expressionTests =
    [ ("5 + 5", BinE (LiteralE (IntExpr 5) `Add` LiteralE (IntExpr 5)))
    , ("5 * 5", BinE (LiteralE (IntExpr 5) `Mul` LiteralE (IntExpr 5)))
    , ("5 - 5", BinE (LiteralE (IntExpr 5) `Sub` LiteralE (IntExpr 5)))
    , ("5 / 5", BinE (LiteralE (IntExpr 5) `Div` LiteralE (IntExpr 5)))
    , ("5 % 5", BinE (LiteralE (IntExpr 5) `Mod` LiteralE (IntExpr 5)))
    , ("-5"   , UnaryE (Negate (LiteralE (IntExpr 5))))
    , ( "\"a\" + \"b\""
      , BinE (LiteralE (StringExpr "a") `Add` LiteralE (StringExpr "b"))
      )
    , ("10(5)", Call (LiteralE (IntExpr 10)) [LiteralE (IntExpr 5)])
    , ("func(5)", Call (IdentifierE "func") [LiteralE (IntExpr 5)])
    , ("func(5)(\"hi\")"
      , Call
            (Call (IdentifierE "func") [LiteralE (IntExpr 5)])
            [LiteralE (StringExpr "hi")]
      )
    , ( "a.b",     IdentifierE "a" `AccessE` "b")
    , ( "a().b",   Call (IdentifierE "a") [] `AccessE` "b")
    , ( "(1+4).b", BinE (LiteralE (IntExpr 1) `Add` LiteralE (IntExpr 4)) `AccessE` "b")
    , ( "a.b.c",   (IdentifierE "a" `AccessE` "b") `AccessE` "c")
    , ( "fun () -> Unit {}", FuncExpr [] Nothing "Unit" (Block []))
    , ( "fun (a: Int) -> Unit {}", FuncExpr [("a", "Int")] Nothing "Unit" (Block []))
    , ( "fun (a: Int, b: Int) -> Unit {}", FuncExpr [("a", "Int"), ("b", "Int")] Nothing "Unit" (Block []))
    , ( "fun () => eff -> Unit {}", FuncExpr [] (Just $ fromList ["eff"]) "Unit" (Block []))
    , ( "fun (a: Int) => eff -> Unit {}", FuncExpr [("a", "Int")] (Just $ fromList ["eff"]) "Unit" (Block []))
    , ( "fun (a: Int, b: Int) => eff -> Unit {}", FuncExpr [("a", "Int"), ("b", "Int")] (Just $ fromList ["eff"]) "Unit" (Block []))
    ]

statementTests :: [(String, AST 'Stmt Parsed)]
statementTests =
    [ ("while True { print(name); }", While (
        WhileStmt (LiteralE $ BoolExpr True) $ Block [
            ExprStmt $ Call (IdentifierE "print") [IdentifierE "name"]
        ]))
    , ("if True { print(name1); } else { print(name2); }", If (
        IfStmt (LiteralE $ BoolExpr True) (Block [
            ExprStmt $ Call (IdentifierE "print") [IdentifierE "name1"]
        ]) $ Block [
            ExprStmt $ Call (IdentifierE "print") [IdentifierE "name2"]
        ]
        ))
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
    , ( "(a) => eff -> c"
      , ParsedSignatureType [ParsedTypeName "a"]
                            (Just $ fromList ["eff"])
                            (ParsedTypeName "c")
      )
    , ( "(a) => {eff1, eff2} -> c"
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
    , ( "(a) => eff -> (b) -> c"
      , ParsedSignatureType
          [ParsedTypeName "a"]
          (Just $ fromList ["eff"])
          (ParsedSignatureType [ParsedTypeName "b"] Nothing (ParsedTypeName "c")
          )
      )
    , ( "(a) -> (b) => eff -> c"
      , ParsedSignatureType
          [ParsedTypeName "a"]
          Nothing
          (ParsedSignatureType [ParsedTypeName "b"]
                               (Just $ fromList ["eff"])
                               (ParsedTypeName "c")
          )
      )
    , ( "(a) => eff1 -> (b) => eff2 -> c"
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
parsedBy str p = case runParser' p (initialState str) of
    (_, Left  err) -> Left err
    (s, Right res) -> Right (stateInput s, res)

shouldParseTo
    :: (Eq a, Show a, Show s, Show (Token s), Show e)
    => ParseTestResult e s a
    -> a
    -> Expectation
shouldParseTo (Right (_, actual)) expected = actual `shouldBe` expected
shouldParseTo (Left err) _ = expectationFailure ("Got error " <> show err)

shouldLeave
    :: (Eq s, Show a, Show s, Show (Token s), Show e)
    => ParseTestResult e s a
    -> s
    -> Expectation
shouldLeave (Right (s, _)) expected = s `shouldBe` expected
shouldLeave (Left err) _ = expectationFailure ("Got error " <> show err)

-- | Given input for parsing, construct initial state for parser.
initialState :: s -> State s
initialState s =
    State { stateInput = s, stateOffset = 0, statePosState = initialPosState s }

-- | Given input for parsing, construct initial positional state.
initialPosState :: s -> PosState s
initialPosState s = PosState { pstateInput      = s
                             , pstateOffset     = 0
                             , pstateSourcePos  = initialPos ""
                             , pstateTabWidth   = defaultTabWidth
                             , pstateLinePrefix = ""
                             }
