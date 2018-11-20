module Kima.Frontend.Parser where

import Prelude hiding (mod) 

import Kima.AST
import Kima.Frontend.Tokenizer hiding (Mod)
import Kima.Frontend.Types
import qualified Kima.Frontend.Tokenizer as T 

import Text.Megaparsec hiding (dbg)
import Text.Megaparsec.Expr

program :: Parser (ParsedAST 'TopLevel)
program = Program <$> some funcDef <* eof

-- Function defintions

funcDef :: Parser (ParsedAST 'FunctionDef)
funcDef = do 
    reserved RFun 
    name <- identifier
    args <- parens typedArgList
    
    symbol Arrow
    retType <- typeExpr
    FuncDefAnn name args retType <$> block
    <?> "Function definition"

typedArgList :: Parser [(ParsedName, TypeExpr)]
typedArgList = typedArg `sepBy` symbol Comma
    <?> "Argument list"

typedArg :: Parser (ParsedName, TypeExpr)
typedArg = (,) <$> identifier <*> (symbol Colon *> typeExpr)

-- Statements

stmt :: Parser (ParsedAST 'Stmt)
stmt = letStmt <|> varStmt <|> whileStmt <|> assignStmt <|> exprStmt <|> ifStmt
    <?> "statement"

block :: Parser (ParsedAST 'Stmt)
block = Block <$> braces (stmt `sepEndBy` stmtEnd)
    <?> "Block"

letStmt :: Parser (ParsedAST 'Stmt)
letStmt = Let
    <$> (reserved RLet *> identifier) 
    <*> (symbol Colon *> typeExpr) 
    <*> (symbol Equals *> expr)
    <?> "let statement"

varStmt :: Parser (ParsedAST 'Stmt)
varStmt = Var 
    <$> (reserved RVar *> identifier) 
    <*> (symbol Colon *> typeExpr) 
    <*> (symbol Equals *> expr)
    <?> "var statement"

assignStmt :: Parser (ParsedAST 'Stmt)
assignStmt = try (Assign
    <$> identifier 
    <*> (symbol Equals *> expr))
    <?> "assignment"

whileStmt :: Parser (ParsedAST 'Stmt)
whileStmt = While <$> (WhileStmt
    <$> (reserved RWhile *> expr)
    <*> block)
    <?> "while statement"

exprStmt :: Parser (ParsedAST 'Stmt)
exprStmt = ExprStmt <$> expr
    <?> "expression statement"

ifStmt :: Parser (ParsedAST 'Stmt)
ifStmt = If <$> (IfStmt <$> expr <*> stmt <*> stmt)

-- Expressions

expr :: Parser (ParsedAST 'Expr)
expr = makeExprParser term [
        [ prefix (symbol Minus) (UnaryE . Negate)
        , prefix (symbol Plus) id
        , prefix (symbol Bang) (UnaryE . Invert)
        ],
        [ binary (symbol Plus)  (\l r -> BinE $ Add l r)
        , binary (symbol Minus) (\l r -> BinE $ Sub l r)
        , binary (symbol Star)  (\l r -> BinE $ Mul l r)
        , binary (symbol Slash) (\l r -> BinE $ Div l r)
        , binary (symbol T.Mod) (\l r -> BinE $ Mod l r)
        ]
    ] <?> "expression"
    
binary  p f = InfixL  (f <$ p)
prefix  p f = Prefix  (f <$ p)
postfix p f = Postfix (f <$ p)

term :: Parser (ParsedAST 'Expr) 
term = try call <|> baseterm

-- | A term without calls (Useful for parsing calls)
baseterm :: Parser (ParsedAST 'Expr)
baseterm = parens expr
   <|> LiteralE . StringExpr     <$> string
   <|> LiteralE . IntExpr        <$> intLiteral
   <|> LiteralE . FloatExpr      <$> floatLiteral
   <|>            Identifier     <$> identifier

argList :: Parser [ParsedAST 'Expr]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls
call :: Parser (ParsedAST 'Expr)
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
