module Frontend.Parser where

import Prelude hiding (mod)
import Data.Newtype

import Text.Megaparsec
import Text.Megaparsec.Expr

import Frontend.Tokenizer hiding (Mod)
import qualified Frontend.Tokenizer as T 
import Frontend.Types
import AST

program :: Parser (Program Stmt)
program = Program <$> some funcDef <* eof

-- Function defintions

funcDef :: Parser (FuncDef Stmt)
funcDef = do 
    reserved RFun 
    name <- identifier
    args <- parens argList
    
    symbol Arrow
    retType <- typeExpr
    FuncDef name (NamedSignature args retType) <$> block
    <?> "Function definition"

argList :: Parser [(Name, TypeExpr)]
argList = typedArg `sepBy` symbol Comma
    <?> "Argument list"

typedArg :: Parser (Name, TypeExpr)
typedArg = (,) <$> identifier <*> (symbol Colon *> typeExpr)

-- Statements

stmt :: Parser Stmt
stmt = letStmt <|> varStmt <|> whileStmt <|> assignStmt <|> exprStmt
    <?> "statement"

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
assignStmt = Stmt <$> (iAssignStmt 
    <$> identifier 
    <*> (symbol Equals *> expr)
    <?> "assignment")

whileStmt :: Parser Stmt
whileStmt = Stmt <$> (iWhileStmt 
    <$> (reserved RWhile *> expr)
    <*> (Block . (fmap unwrap) . unBlock <$> block)
    <?> "while statement")

exprStmt :: Parser Stmt
exprStmt = Stmt <$> (iExprStmt <$> expr)
    <?> "expression statement"

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
term = (Expr . iIdentifierExpr <$> identifier )
   <|> (Expr . iStringExpr <$> string)
   <|> parens expr

-- Types

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

block :: Parser (Block Stmt)
block = Block <$> braces (stmt `sepEndBy` stmtEnd)
    <?> "Block"
