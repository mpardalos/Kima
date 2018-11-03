module Frontend.Parser where

import Prelude hiding (mod)

import AST

import Data.Newtype

import Frontend.Tokenizer hiding (Mod)
import Frontend.Types
import qualified Frontend.Tokenizer as T 

import Text.Megaparsec hiding (dbg)
import Text.Megaparsec.Expr


program :: Parser (Program Stmt)
program = Program <$> some funcDef <* eof

-- Function defintions

funcDef :: Parser (FuncDef Stmt)
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
block = Stmt . iBlockStmt . fmap unwrap <$> braces (stmt `sepEndBy` stmtEnd)
    <?> "Block"

letStmt :: Parser Stmt
letStmt = Stmt <$> (iLetStmt
    <$> (reserved RLet *> identifier) 
    <*> (symbol Colon *> typeExpr) 
    <*> (symbol Equals *> expr)
    <?> "let statement")

varStmt :: Parser Stmt
varStmt = Stmt <$> (iVarStmt 
    <$> (reserved RVar *> identifier) 
    <*> (symbol Colon *> typeExpr) 
    <*> (symbol Equals *> expr)
    <?> "var statement")

assignStmt :: Parser Stmt
assignStmt = try $ Stmt <$> (iAssignStmt 
    <$> identifier 
    <*> (symbol Equals *> expr)
    <?> "assignment")

whileStmt :: Parser Stmt
whileStmt = Stmt <$> (iWhileStmt 
    <$> (reserved RWhile *> expr)
    <*> (unwrap <$> block)
    <?> "while statement")

exprStmt :: Parser Stmt
exprStmt = Stmt <$> (iExprStmt <$> expr)
    <?> "expression statement"

ifStmt :: Parser Stmt
ifStmt = Stmt <$> (iIfStmt 
    <$> expr 
    <*> (unwrap <$> stmt) 
    <*> (unwrap <$> stmt))

-- Expressions

expr :: Parser Expr
expr = makeExprParser term [
        [ prefix (symbol Minus) (under (iNegate :: ExprTerm -> ExprTerm))
        , prefix (symbol Plus) id
        , prefix (symbol Bang) (under (iInvert :: ExprTerm -> ExprTerm))
        ],
        [ binary (symbol Plus) (under2 (iAdd :: ExprTerm -> ExprTerm -> ExprTerm))
        , binary (symbol Minus) (under2 (iSub :: ExprTerm -> ExprTerm -> ExprTerm))
        , binary (symbol Star) (under2 (iMul :: ExprTerm -> ExprTerm -> ExprTerm))
        , binary (symbol Slash) (under2 (iDiv :: ExprTerm -> ExprTerm -> ExprTerm))
        , binary (symbol T.Mod) (under2 (iMod :: ExprTerm -> ExprTerm -> ExprTerm))
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
   <|> Expr . iStringExpr     <$> string
   <|> Expr . iIntExpr        <$> intLiteral
   <|> Expr . iFloatExpr      <$> floatLiteral
   <|> Expr . iIdentifierExpr <$> identifier

argList :: Parser [Expr]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls
call :: Parser Expr
call = do 
    callee :: Expr <- baseterm 
    argLists :: [[Expr]] <- some argList
    return (foldl makeCallExpr callee argLists)
    where
        makeCallExpr :: Expr -> [Expr] -> Expr
        makeCallExpr (Expr callee) args = Expr (iCallExpr callee (unwrap <$> args))

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
