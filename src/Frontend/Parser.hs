module Frontend.Parser where

import Prelude hiding (mod)

import Text.Megaparsec
import Text.Megaparsec.Expr

import Frontend.Tokenizer hiding (Mod)
import qualified Frontend.Tokenizer as T 
import Frontend.Types
import CommonTypes

program :: Parser [FuncDef]
program = some funcDef <* eof

-- Function defintions

funcDef :: Parser FuncDef
funcDef = do 
    reserved RFun 
    name <- identifier
    args <- parens argList
    
    symbol Arrow
    retType <- typeExpr
    FuncDef name args retType <$> block
    <?> "Function definition"

argList :: Parser [(TypeExpr, Name)]
argList = typedArg `sepBy` symbol Comma
    <?> "Argument list"

typedArg :: Parser (TypeExpr, Name)
typedArg = do 
    n <- identifier
    symbol Colon
    t <- typeExpr
    return (t, n)

-- Statements

stmt :: Parser Stmt
stmt = letStmt <|> varStmt <|> whileStmt <|> assignStmt <|> exprStmt
    <?> "statement"

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
assignStmt = AssignStmt 
    <$> identifier 
    <*> (symbol Equals *> expr)
    <?> "assignment"

whileStmt :: Parser Stmt
whileStmt = WhileStmt 
    <$> (reserved RWhile *> expr)
    <*> block
    <?> "while statement"

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr
    <?> "expression statement"

-- Expressions

expr :: Parser Expr
expr = makeExprParser term [
        [ prefix (symbol Minus) (UnaryExpr Negate)
        , prefix (symbol Plus) id
        , prefix (symbol Bang) (UnaryExpr Invert)
        ],
        [ binary (symbol Plus) (BinExpr Add)
        , binary (symbol Minus) (BinExpr Sub)
        , binary (symbol Star) (BinExpr Mul)
        , binary (symbol Slash) (BinExpr Div)
        , binary (symbol T.Mod) (BinExpr Mod)
        ]
    ] <?> "expression"
    
binary  p f = InfixL  (f <$ p)
prefix  p f = Prefix  (f <$ p)
postfix p f = Postfix (f <$ p)

term :: Parser Expr
term = IdentifierExpr <$> identifier 
   <|> StringExpr <$> string 
   <|> parens expr

-- Types

typeExpr :: Parser TypeExpr
typeExpr = SignatureType <$> try signature
       <|> TypeName <$> try identifier
       <?> "type expression"

signature :: Parser Signature
signature = Signature 
    <$> parens (typeExpr `sepBy` symbol Comma) 
    <*> (symbol Arrow *> typeExpr)
    <?> "function signature"

typeName :: Parser TypeExpr
typeName = TypeName <$> identifier

effectExpr :: Parser EffectExpr
effectExpr = EffectName <$> identifier

block :: Parser Block
block = Block <$> braces (stmt `sepEndBy` stmtEnd)
    <?> "Block"
