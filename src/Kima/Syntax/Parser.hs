{-# LANGUAGE OverloadedLists #-}
module Kima.Syntax.Parser where

import           Prelude                 hiding ( mod )

import           Kima.AST
import           Kima.Syntax.Tokenizer   hiding ( Mod )
import qualified Kima.Syntax.Tokenizer         as T
                                                ( Symbol(Mod) )
import           Kima.Syntax.Types

import           Control.Monad.Combinators.Expr
import           Data.Functor
import           GHC.Exts
import           Text.Megaparsec

program :: Parser (Module Parsed)
program = Program <$> (whitespace *> some topLevel <* eof)

-- Function defintions
topLevel :: Parser (TopLevel Parsed)
topLevel = funcDef <|> dataDef <|> try effectSynonymDef <|> operationDef

effectSpec :: Parser ParsedEffect
effectSpec = try singleEffect <|> bracedEffects
  where
    singleEffect  = EffectNames . pure @[] <$> identifier
    bracedEffects = EffectNames <$> braces (identifier `sepBy` symbol Comma)

functionReturn :: Parser (Maybe ParsedEffect, Maybe ParsedTypeExpr)
functionReturn =
    try effectAndReturnType
        <|> try justReturnType
        <|> try justEffectType
        <|> neither
  where
    effectAndReturnType = label "both effect and return type" $ do
        effects    <- symbol Colon *> effectSpec
        returnType <- symbol Arrow *> typeExpr
        return (Just effects, Just returnType)

    justReturnType =
        (symbol Arrow *> typeExpr)
            <&> (\rt -> (Nothing, Just rt))
            <?> "just return type"

    justEffectType =
        (symbol Colon *> effectSpec)
            <&> (\eff -> (Just eff, Nothing))
            <?> "just effect type"

    neither = label "No return spec" $ pure (Nothing, Nothing)

funcDef :: Parser (TopLevel Parsed)
funcDef = label "Function definition" $ do
    reserved RFun
    pIdentifier            <- identifier
    pArgs                  <- typedArgList
    (pEffect, pReturnType) <- functionReturn
    pBody                  <- block

    return (FuncDef pIdentifier pArgs pEffect pReturnType pBody)


dataDef :: Parser (TopLevel Parsed)
dataDef =
    reserved RData
        *>  (DataDef <$> identifier <*> typedArgList)
        <?> "Datatype declaration"

typedArgList :: Parser [(Name, Maybe ParsedTypeExpr)]
typedArgList = parens (typedArg `sepBy` symbol Comma) <?> "Argument list"

typedArg :: IsString s => Parser (s, Maybe ParsedTypeExpr)
typedArg = (,) <$> identifier <*> optional (symbol Colon *> typeExpr)

-- Statements

stmt :: Parser (Stmt Parsed)
stmt =
    letStmt
        <|> varStmt
        <|> whileStmt
        <|> ifStmt
        <|> assignStmt
        <|> block
        <|> exprStmt
        <?> "statement"

block :: Parser (Stmt Parsed)
block = Block <$> braces (stmt `sepEndBy` stmtEnd) <?> "Block"

letStmt :: Parser (Stmt Parsed)
letStmt =
    Let
        <$> (reserved RLet *> identifier)
        <*> optional (symbol Colon *> typeExpr)
        <*> (symbol Equals *> expr)
        <?> "let statement"

varStmt :: Parser (Stmt Parsed)
varStmt =
    Var
        <$> (reserved RVar *> identifier)
        <*> optional (symbol Colon *> typeExpr)
        <*> (symbol Equals *> expr)
        <?> "var statement"

assignStmt :: Parser (Stmt Parsed)
assignStmt =
    try (Assign <$> writeAccess <*> (symbol Equals *> expr)) <?> "assignment"

writeAccess :: IsString s => Parser (WriteAccess s)
writeAccess = label "accessor" $ do
    base   <- identifier
    fields <- option [] (symbol Dot *> identifier `sepBy1` symbol Dot)
    return (WriteAccess base fields)

whileStmt :: Parser (Stmt Parsed)
whileStmt =
    While
        <$> (WhileStmt <$> (reserved RWhile *> expr) <*> block)
        <?> "while statement"

exprStmt :: Parser (Stmt Parsed)
exprStmt = ExprStmt <$> expr <?> "expression statement"

ifStmt :: Parser (Stmt Parsed)
ifStmt =
    If
        <$> (   IfStmt
            <$> (reserved RIf *> expr)
            <*> block
            <*> (reserved RElse *> stmt)
            )

-- Expressions

expr :: Parser (Expr Parsed)
expr =
    makeExprParser
            term
            [ [ prefix (symbol Minus) (UnaryE . Negate)
              , prefix (symbol Plus)  id
              , prefix (symbol Bang)  (UnaryE . Invert)
              ]
            , [ binary (symbol Plus)         ((BinE .) . Add)
              , binary (symbol Minus)        ((BinE .) . Sub)
              , binary (symbol Star)         ((BinE .) . Mul)
              , binary (symbol Slash)        ((BinE .) . Div)
              , binary (symbol T.Mod)        ((BinE .) . Mod)
              , binary (symbol GreaterThan)  ((BinE .) . Greater)
              , binary (symbol GreaterEqual) ((BinE .) . GreatEq)
              , binary (symbol LessThan)     ((BinE .) . Less)
              , binary (symbol LessEqual)    ((BinE .) . LessEq)
              , binary (symbol EqualsEquals) ((BinE .) . Eq)
              ]
            ]
        <?> "expression"

binary p f = InfixL (f <$ p)
prefix p f = Prefix (f <$ p)
postfix p f = Postfix (f <$ p)

term :: Parser (Expr Parsed)
term = try handlerExpr <|> try accessCall <|> try funcExpr <|> try baseTerm

funcExpr :: Parser (Expr Parsed)
funcExpr = do
    reserved RFun
    pArgs                  <- typedArgList
    (pEffect, pReturnType) <- functionReturn
    body                   <- block
    return (FuncExpr pArgs pEffect pReturnType body)

-- | A term without calls (Useful for parsing calls)
baseTerm :: Parser (Expr Parsed)
baseTerm =
    parens expr
        <|> LiteralE
        .   StringExpr
        <$> try string
        <|> LiteralE
        .   FloatExpr
        <$> try floatLiteral
        <|> LiteralE
        .   IntExpr
        <$> try intLiteral
        <|> LiteralE
        .   BoolExpr
        <$> try boolLiteral
        <|> IdentifierE
        .   Identifier
        <$> try identifier

argList :: Parser [Expr Parsed]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls and accesses (a.b)
accessCall :: Parser (Expr Parsed)
accessCall = do
    callee   <- baseTerm
    argLists <- some callOrAccess
    return (foldl combiner callee argLists)
  where
    callOrAccess = Left <$> (symbol Dot *> identifier) <|> Right <$> argList

    combiner acc (Left  attr) = AccessE acc attr
    combiner acc (Right args) = Call acc args

handlerExpr :: Parser (Expr Parsed)
handlerExpr = do
    reserved RHandle
    handledExpr <- expr
    handlers <- braces (many handlerClause)
    return (Handle handledExpr handlers)

handlerClause :: Parser (HandlerClause Parsed)
handlerClause = HandlerClause
    <$> identifier
    <*> typedArgList
    <*> optional (symbol Arrow *> typeExpr)
    <*> block

typeExpr :: Parser ParsedTypeExpr
typeExpr =
    try anonymousSignature
        <|> (ParsedTypeName <$> identifier)
        <?> "type expression"
  where
    anonymousSignature = label "function signature" $ do
        arguments  <- parens (typeExpr `sepBy` symbol Comma)
        effect     <- optional (symbol Colon *> effectSpec)
        returnType <- symbol Arrow *> typeExpr

        return (ParsedSignatureType arguments effect returnType)

operationDef :: Parser (TopLevel Parsed)
operationDef =
    reserved REffect
        *> (   OperationDef
           <$> identifier
           <*> typedArgList
           <*> (Just <$> (symbol Arrow *> typeExpr))
           )

effectSynonymDef :: Parser (TopLevel Parsed)
effectSynonymDef =
    reserved REffect
        *> (EffectSynonymDef <$> identifier <*> braces
               (identifier `sepBy` symbol Comma)
           )
