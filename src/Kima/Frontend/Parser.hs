module Kima.Frontend.Parser where

import Prelude hiding (mod)

import Kima.AST
import Kima.Frontend.Tokenizer hiding (Mod)
import qualified Kima.Frontend.Tokenizer as T (Symbol(Mod))
import Kima.Frontend.Types

import Control.Monad.Combinators.Expr
import GHC.Exts
import Text.Megaparsec

program :: Parser (AST 'Module Parsed)
program = Program <$> (whitespace *> some topLevel <* eof)

-- Function defintions
topLevel :: Parser (AST 'TopLevel Parsed)
topLevel = funcDef <|> dataDef

funcDef :: Parser (AST 'TopLevel Parsed)
funcDef = reserved RFun *> (
    FuncDef
    <$> identifier
    <*> parens typedArgList
    <*> (symbol Arrow *> typeExpr)
    <*> block)

dataDef :: Parser (AST 'TopLevel Parsed)
dataDef = reserved RData *> (
    DataDef
    <$> identifier
    <*> parens (typedArg `sepBy` symbol Comma))
    <?> "Datatype declaration"

typedArgList :: Parser [(Name, TypeExpr)]
typedArgList = typedArg `sepBy` symbol Comma
    <?> "Argument list"

typedArg :: IsString s => Parser (s, TypeExpr)
typedArg = (,) <$> identifier <*> (symbol Colon *> typeExpr)

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
    <*> (symbol Colon *> typeExpr)
    <*> (symbol Equals *> expr)
    <?> "let statement"

varStmt :: Parser (AST 'Stmt Parsed)
varStmt = Var
    <$> (reserved RVar *> identifier)
    <*> (symbol Colon *> typeExpr)
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
term = try accessCall <|> funcExpr <|> baseTerm

funcExpr :: Parser (AST 'Expr Parsed)
funcExpr = reserved RFun *> (
    FuncExpr
    <$> parens typedArgList
    <*> (symbol Arrow *> typeExpr)
    <*> block)

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
            Right <$> parens (expr `sepBy` symbol Comma)

        combiner acc (Left  attr) = AccessE acc attr
        combiner acc (Right args) = Call acc args

typeExpr :: Parser TypeExpr
typeExpr = uncurry SignatureType <$> try anonymousSignature
       <|> (TypeName <$> try identifier)
       <?> "type expression"

anonymousSignature :: Parser ([TypeExpr], TypeExpr)
anonymousSignature = (,)
    <$> parens (typeExpr `sepBy` symbol Comma)
    <*> (symbol Arrow *> typeExpr)
    <?> "function signature"

typeName :: Parser TypeExpr
typeName = TypeName <$> identifier
