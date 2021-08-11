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
program = Module <$> (whitespace *> some topLevel <* eof)

-- Function defintions
topLevel :: Parser (TopLevel Parsed)
topLevel = funcDef <|> dataDef <|> effectDef

effectSpec :: Parser ParsedEffect
effectSpec = singleEffect <|> bracedEffects
  where
    singleEffect  = EffectNames . pure @[] <$> identifier
    bracedEffects = EffectNames <$> braces (identifier `sepBy` symbol Comma)

functionReturn :: Parser (Maybe ParsedEffect, Maybe ParsedTypeExpr)
functionReturn =
    effectAndReturnType
        <|> justReturnType
        <|> justEffectType
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
dataDef = do
  reserved RData
  name <- identifier
  sumDef name <|> productDef name
  where
    sumDef :: Name -> Parser (TopLevel Parsed)
    sumDef name = SumTypeDef name <$> braces (constructor `sepBy` symbol Comma)
      where
        constructorArgs = option [] $ parens ((Just <$> typeExpr) `sepBy` symbol Comma)

        constructor :: Parser (Name, [Maybe ParsedTypeExpr])
        constructor = (,) <$> identifier <*> constructorArgs

    productDef name = ProductTypeDef name <$> typedArgList

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
        <|> breakStmt
        <|> block
        <|> exprStmt
        <?> "statement"

block :: Parser (Stmt Parsed)
block = BlockStmt <$> braces (stmt `sepEndBy` stmtEnd) <?> "BlockStmt"

letStmt :: Parser (Stmt Parsed)
letStmt =
    LetStmt
        <$> (reserved RLet *> identifier)
        <*> optional (symbol Colon *> typeExpr)
        <*> (symbol Equals *> expr)
        <?> "let statement"

varStmt :: Parser (Stmt Parsed)
varStmt =
    VarStmt
        <$> (reserved RVar *> identifier)
        <*> optional (symbol Colon *> typeExpr)
        <*> (symbol Equals *> expr)
        <?> "var statement"

assignStmt :: Parser (Stmt Parsed)
assignStmt =
    try (AssignStmt <$> writeAccess <*> (symbol Equals *> expr)) <?> "assignment"

writeAccess :: IsString s => Parser (WriteAccess s)
writeAccess = label "accessor" $ do
    base   <- identifier
    fields <- option [] (symbol Dot *> identifier `sepBy1` symbol Dot)
    return (WriteAccess base fields)

whileStmt :: Parser (Stmt Parsed)
whileStmt =
    WhileStmt
        <$> (While <$> (reserved RWhile *> expr) <*> block)
        <?> "while statement"

breakStmt :: Parser (Stmt Parsed)
breakStmt = label "break statement" $ do
    reserved RBreak
    maybeExpr <- optional expr

    case maybeExpr of
        Just e  -> return (BreakStmt e)
        Nothing -> return SimpleBreakStmt

exprStmt :: Parser (Stmt Parsed)
exprStmt = ExprStmt <$> expr <?> "expression statement"

ifStmt :: Parser (Stmt Parsed)
ifStmt = do
    reserved RIf
    cond          <- expr
    ifBody        <- block
    maybeElseBody <- optional (reserved RElse *> stmt)

    case maybeElseBody of
        Just elseBody -> return (IfStmt (If cond ifBody elseBody))
        Nothing       -> return (SimpleIfStmt cond ifBody)

-- Expressions

expr :: Parser (Expr Parsed)
expr =
    makeExprParser
            term
            [ [ prefix (symbol Minus) (UnaryExpr NegateOp)
              , prefix (symbol Plus)  id
              , prefix (symbol Bang)  (UnaryExpr InvertOp)
              ]
            , [ binary (symbol Plus)         (BinExpr AddOp)
              , binary (symbol Minus)        (BinExpr SubOp)
              , binary (symbol StarStar)     (BinExpr PowOp)
              , binary (symbol Star)         (BinExpr MulOp)
              , binary (symbol Slash)        (BinExpr DivOp)
              , binary (symbol T.Mod)        (BinExpr ModOp)
              , binary (symbol GreaterThan)  (BinExpr GTOp)
              , binary (symbol GreaterEqual) (BinExpr GTEOp)
              , binary (symbol LessThan)     (BinExpr LTOp)
              , binary (symbol LessEqual)    (BinExpr LTEOp)
              , binary (symbol EqualsEquals) (BinExpr EqualsOp)
              ]
            ]
        <?> "expression"

binary p f = InfixL (f <$ p)
prefix p f = Prefix (f <$ p)
postfix p f = Postfix (f <$ p)

term :: Parser (Expr Parsed)
term = matchExpr <|> handlerExpr <|> accessCall <|> funcExpr <|> baseTerm

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
    try (LiteralExpr UnitLit <$ unitLiteral)
        <|> parens expr
        <|> (LiteralExpr . StringLit <$> string)
        <|> (LiteralExpr . BoolLit <$> boolLiteral)
        <|> (LiteralExpr . FloatLit <$> try floatLiteral)
        <|> (LiteralExpr . IntLit <$> try intLiteral)
        <|> (IdentifierExpr . Identifier <$> identifier)

argList :: Parser [Expr Parsed]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls and accesses (a.b)
accessCall :: Parser (Expr Parsed)
accessCall = do
    (callee, argLists) <- try $ do
        callee   <- baseTerm
        argLists <- some callOrAccess
        pure (callee, argLists)
    return (foldl combiner callee argLists)
  where
    callOrAccess = Left <$> (symbol Dot *> identifier) <|> Right <$> argList
        <?> "Call"

    combiner acc (Left  attr) = AccessExpr acc attr
    combiner acc (Right args) = CallExpr acc args

matchExpr :: Parser (Expr Parsed)
matchExpr =
  reserved RMatch
    *> (MatchExpr <$> expr <*> braces (many matchClause))
    <?> "Match expression"
  where
    matchClause = MatchClause <$> matchPattern <*> block

    matchPattern =
      try constructorPattern
        <|> (WildcardPattern <$> identifier)

    constructorPattern =
      ConstructorPattern
        <$> identifier
        <*> parens (matchPattern `sepBy` symbol Comma)

handlerExpr :: Parser (Expr Parsed)
handlerExpr =
        reserved RHandle
        *> (simpleHandlerExpr <|> fullHandlerExpr)
  where
    simpleHandlerExpr = do
        handledExpr <- expr <?> "Handled expression"
        handlers    <- braces (many handlerClause) <?> "Handler list"
        return (SimpleHandleExpr handledExpr handlers)

    fullHandlerExpr = do
        handledBlock <- block <?> "Handle block"
        reserved RWith
        handlers <- braces (many handlerClause) <?> "Handler list"
        return (HandleExpr handledBlock handlers)

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


effectDef :: Parser (TopLevel Parsed)
effectDef = do
    reserved REffect
    name <- identifier
    operation name <|> effectSynonym name
    where
        operation name = do
            args <- typedArgList
            symbol Arrow
            rt <- typeExpr
            return (OperationDef name args (Just rt))

        effectSynonym name = do
            ops <- braces (identifier `sepBy` symbol Comma)
            return (EffectSynonymDef name ops)
