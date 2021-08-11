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
dataDef = try sumDef <|> productDef

sumDef :: Parser (TopLevel Parsed)
sumDef =
  reserved RData
    *> ( SumTypeDef
           <$> identifier
           <*> braces (constructor `sepBy` symbol Comma)
       )
  where
    constructorArgs = option [] $ parens ((Just <$> typeExpr) `sepBy` symbol Comma)

    constructor :: Parser (Name, [Maybe ParsedTypeExpr])
    constructor = (,) <$> identifier <*> constructorArgs

productDef :: Parser (TopLevel Parsed)
productDef =
    reserved RData
        *>  (ProductTypeDef <$> identifier <*> typedArgList)
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
term = matchExpr <|> try handlerExpr <|> try accessCall <|> try funcExpr <|> try baseTerm

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
    try (parens expr)
        <|> (LiteralExpr . StringLit <$> try string)
        <|> (LiteralExpr . FloatLit <$> try floatLiteral)
        <|> (LiteralExpr . IntLit <$> try intLiteral)
        <|> (LiteralExpr . BoolLit <$> try boolLiteral)
        <|> (LiteralExpr UnitLit <$ try unitLiteral)
        <|> (IdentifierExpr . Identifier <$> try identifier)

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
