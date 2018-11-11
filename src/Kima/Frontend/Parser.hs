module Kima.Frontend.Parser where

import Prelude hiding (mod) 

import Kima.AST.Parsed as P
import Kima.AST.Common
import Kima.AST.Expression
import Kima.Frontend.Tokenizer hiding (Mod)
import Kima.Frontend.Types
import qualified Kima.Frontend.Tokenizer as T 

import Text.Megaparsec hiding (dbg)
import Text.Megaparsec.Expr


program :: Parser Program
program = Program <$> some funcDef <* eof

-- Function defintions

funcDef :: Parser P.FuncDef
funcDef = do 
    reserved RFun 
    name <- identifier
    args <- parens typedArgList
    
    symbol Arrow
    retType <- typeExpr
    FuncDef name (NamedSignature args retType) <$> block
    <?> "Function definition"

typedArgList :: Parser [(Name, TypeExpr)]
typedArgList = typedArg `sepBy` symbol Comma
    <?> "Argument list"

typedArg :: Parser (Name, TypeExpr)
typedArg = (,) <$> identifier <*> (symbol Colon *> typeExpr)

-- Statements

stmt :: Parser Stmt
stmt = letStmt <|> varStmt <|> whileStmt <|> assignStmt <|> exprStmt <|> ifStmt
    <?> "statement"

block :: Parser Stmt
block = BlockStmt <$> braces (stmt `sepEndBy` stmtEnd)
    <?> "Block"

letStmt :: Parser Stmt
letStmt = LetStmt
    <$> (reserved RLet *> identifier) 
    <*> (symbol Colon *> typeExpr) 
    <*> (symbol Equals *> expr)
    <?> "let statement"

varStmt :: Parser Stmt
varStmt = VarStmt 
    <$> (reserved RVar *> identifier) 
    <*> (symbol Colon *> typeExpr) 
    <*> (symbol Equals *> expr)
    <?> "var statement"

assignStmt :: Parser Stmt
assignStmt = try (AssignStmt 
    <$> identifier 
    <*> (symbol Equals *> expr))
    <?> "assignment"

whileStmt :: Parser Stmt
whileStmt = WhileStmt 
    <$> (reserved RWhile *> expr)
    <*> block
    <?> "while statement"

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr
    <?> "expression statement"

ifStmt :: Parser Stmt
ifStmt = IfStmt <$> expr <*> stmt <*> stmt

-- Expressions

expr :: Parser Expr
expr = makeExprParser term [
        [ prefix (symbol Minus) (UnaryExpr . Negate)
        , prefix (symbol Plus) id
        , prefix (symbol Bang) (UnaryExpr . Invert)
        ],
        [ binary (symbol Plus)  (\l r -> BinExpr $ Add l r)
        , binary (symbol Minus) (\l r -> BinExpr $ Sub l r)
        , binary (symbol Star)  (\l r -> BinExpr $ Mul l r)
        , binary (symbol Slash) (\l r -> BinExpr $ Div l r)
        , binary (symbol T.Mod) (\l r -> BinExpr $ Mod l r)
        ]
    ] <?> "expression"
    
binary  p f = InfixL  (f <$ p)
prefix  p f = Prefix  (f <$ p)
postfix p f = Postfix (f <$ p)

term :: Parser Expr 
term = try call <|> baseterm

-- | A term without calls (Useful for parsing calls)
baseterm :: Parser Expr
baseterm = parens expr
   <|> LiteralExpr . StringExpr     <$> string
   <|> LiteralExpr . IntExpr        <$> intLiteral
   <|> LiteralExpr . FloatExpr      <$> floatLiteral
   <|>               Identifier     <$> identifier

argList :: Parser [Expr]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls
call :: Parser Expr
call = do 
    callee <- baseterm 
    argLists <- some argList
    return (foldl Call callee argLists)

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

effectExpr :: Parser EffectExpr
effectExpr = EffectName <$> identifier
