module Kima.Frontend.Parser where

import Prelude hiding (mod) 

import Control.Newtype.Generics

import Kima.AST
import Kima.Frontend.Tokenizer hiding (Mod)
import Kima.Frontend.Types
import qualified Kima.Frontend.Tokenizer as T 

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
block = Stmt . iBlockStmt . fmap unpack <$> braces (stmt `sepEndBy` stmtEnd)
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
    <*> (unpack <$> block)
    <?> "while statement")

exprStmt :: Parser Stmt
exprStmt = Stmt <$> (iExprStmt <$> expr)
    <?> "expression statement"

ifStmt :: Parser Stmt
ifStmt = Stmt <$> (iIfStmt 
    <$> expr 
    <*> (unpack <$> stmt) 
    <*> (unpack <$> stmt))

-- Expressions

expr :: Parser Expr
expr = makeExprParser term [
        [ prefix (symbol Minus) (over Expr iNegate)
        , prefix (symbol Plus) id
        , prefix (symbol Bang) (over Expr iInvert)
        ],
        [ binary (symbol Plus)  (over2 Expr iAdd)
        , binary (symbol Minus) (over2 Expr iSub)
        , binary (symbol Star)  (over2 Expr iMul)
        , binary (symbol Slash) (over2 Expr iDiv)
        , binary (symbol T.Mod) (over2 Expr iMod)
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
        makeCallExpr (Expr callee) args = Expr (iCallExpr callee (unpack <$> args))

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
