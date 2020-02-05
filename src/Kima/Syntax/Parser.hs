{-# LANGUAGE OverloadedLists #-}
module Kima.Syntax.Parser where

import Prelude hiding (mod)

import Kima.AST
import Kima.Syntax.Tokenizer hiding (Mod)
import qualified Kima.Syntax.Tokenizer as T (Symbol(Mod))
import Kima.Syntax.Types

import Control.Monad.Combinators.Expr
import GHC.Exts
import Text.Megaparsec

program :: Parser (AST 'Module Parsed)
program = Program <$> (whitespace *> some topLevel <* eof)

-- Function defintions
topLevel :: Parser (AST 'TopLevel Parsed)
topLevel = funcDef <|> dataDef

effectSpec :: Parser Effect
effectSpec = try singleEffect <|> bracedEffects
    where
        singleEffect = fromEffectNames . pure <$> identifier
        bracedEffects = fromEffectNames <$> braces (identifier `sepBy` symbol Comma)

functionReturn :: Parser (Maybe Effect, Maybe TypeExpr)
functionReturn =
    try effectAndReturnType
        <|> try justReturnType
        <|> try justEffectType
        <|> neither
  where
    effectAndReturnType = label "both effect and return type" $ do
        symbol FatArrow
        effects <- effectSpec
        symbol Arrow
        returnType <- typeExpr
        return (Just effects, Just returnType)

    justReturnType = label "just return type" $ do
        symbol Arrow
        returnType <- typeExpr
        return (Nothing, Just returnType)

    justEffectType = label "just effect type" $ do
        symbol FatArrow
        effect <- effectSpec
        return (Just effect, Nothing)

    neither = label "No return spec" $ pure (Nothing, Nothing)

funcDef :: Parser (AST 'TopLevel Parsed)
funcDef = do
    reserved RFun
    pIdentifier            <- identifier
    pArgs                  <- typedArgList
    (pEffect, pReturnType) <- functionReturn

    pBody <- block
    return (FuncDef pIdentifier pArgs pEffect pReturnType pBody)


dataDef :: Parser (AST 'TopLevel Parsed)
dataDef = reserved RData *> (
    DataDef
    <$> identifier
    <*> typedArgList)
    <?> "Datatype declaration"

typedArgList :: Parser [(Name, Maybe TypeExpr)]
typedArgList = parens (typedArg `sepBy` symbol Comma)
    <?> "Argument list"

typedArg :: IsString s => Parser (s, Maybe TypeExpr)
typedArg = (,) <$> identifier <*> optional (symbol Colon *> typeExpr)

-- Statements

stmt :: Parser (AST 'Stmt Parsed)
stmt = letStmt <|> varStmt <|> whileStmt <|> ifStmt <|> assignStmt <|> exprStmt
    <?> "statement"

block :: Parser (AST 'Stmt Parsed)
block = Block <$> braces (stmt `sepEndBy` stmtEnd)
    <?> "Block"

letStmt :: Parser (AST 'Stmt Parsed)
letStmt = Let
    <$> (reserved RLet *> identifier)
    <*> optional (symbol Colon *> typeExpr)
    <*> (symbol Equals *> expr)
    <?> "let statement"

varStmt :: Parser (AST 'Stmt Parsed)
varStmt = Var
    <$> (reserved RVar *> identifier)
    <*> optional (symbol Colon *> typeExpr)
    <*> (symbol Equals *> expr)
    <?> "var statement"

assignStmt :: Parser (AST 'Stmt Parsed)
assignStmt = try (Assign
    <$> writeAccess
    <*> (symbol Equals *> expr))
    <?> "assignment"

writeAccess :: IsString s => Parser (WriteAccess s)
writeAccess = label "accessor" $ do
    base <- identifier
    fields <- option [] (symbol Dot *> identifier `sepBy1` symbol Dot)
    return (WriteAccess base fields)

whileStmt :: Parser (AST 'Stmt Parsed)
whileStmt = While <$> (WhileStmt
    <$> (reserved RWhile *> expr)
    <*> block)
    <?> "while statement"

exprStmt :: Parser (AST 'Stmt Parsed)
exprStmt = ExprStmt <$> expr
    <?> "expression statement"

ifStmt :: Parser (AST 'Stmt Parsed)
ifStmt = If <$> (IfStmt
    <$> (reserved RIf *> expr)
    <*> block
    <*> (reserved RElse *> block))

-- Expressions

expr :: Parser (AST 'Expr Parsed)
expr = makeExprParser term
    [ [ prefix (symbol Minus) (UnaryE . Negate)
      , prefix (symbol Plus)  id
      , prefix (symbol Bang)  (UnaryE . Invert)
      ]
    , [ binary (symbol Plus)        ((BinE .) . Add)
      , binary (symbol Minus)       ((BinE .) . Sub)
      , binary (symbol Star)        ((BinE .) . Mul)
      , binary (symbol Slash)       ((BinE .) . Div)
      , binary (symbol T.Mod)       ((BinE .) . Mod)
      , binary (symbol GreaterThan) ((BinE .) . Greater)
      , binary (symbol GreaterEqual) ((BinE .) . GreatEq)
      , binary (symbol LessThan) ((BinE .) . Less)
      , binary (symbol LessEqual) ((BinE .) . LessEq)
      , binary (symbol EqualsEquals) ((BinE .) . Eq)
      ]
    ] <?> "expression"

binary  p f = InfixL  (f <$ p)
prefix  p f = Prefix  (f <$ p)
postfix p f = Postfix (f <$ p)

term :: Parser (AST 'Expr Parsed)
term = try accessCall <|> try funcExpr <|> try baseTerm

funcExpr :: Parser (AST 'Expr Parsed)
funcExpr = do
    reserved RFun
    pArgs                  <- typedArgList
    (pEffect, pReturnType) <- functionReturn
    body                   <- block
    return (FuncExpr pArgs pEffect pReturnType body)

-- | A term without calls (Useful for parsing calls)
baseTerm :: Parser (AST 'Expr Parsed)
baseTerm = parens expr
       <|> LiteralE    . StringExpr <$> try string
       <|> LiteralE    . FloatExpr  <$> try floatLiteral
       <|> LiteralE    . IntExpr    <$> try intLiteral
       <|> LiteralE    . BoolExpr   <$> try boolLiteral
       <|> IdentifierE . Identifier <$> try identifier

argList :: Parser [AST 'Expr Parsed]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls and accesses (a.b)
accessCall :: Parser (AST 'Expr Parsed)
accessCall = do
    callee <- baseTerm
    argLists <- some callOrAccess
    return (foldl combiner callee argLists)
    where
        callOrAccess =
            Left  <$> (symbol Dot *> identifier) <|>
            Right <$> argList

        combiner acc (Left  attr) = AccessE acc attr
        combiner acc (Right args) = Call acc args

typeExpr :: Parser TypeExpr
typeExpr = (\(args, eff, rt) -> SignatureType args eff rt) <$> anonymousSignature
       <|> (TypeName <$> identifier)
       <?> "type expression"

anonymousSignature :: Parser ([TypeExpr], Effect, TypeExpr)
anonymousSignature = label "function signature" $ do
    arguments <- parens (typeExpr `sepBy` symbol Comma)
    symbol FatArrow
    effect     <- effectSpec
    symbol Arrow
    returnType <- typeExpr

    return (arguments, effect, returnType)
