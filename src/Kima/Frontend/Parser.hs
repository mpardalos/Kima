module Kima.Frontend.Parser where

import Prelude hiding (mod) 

import Kima.AST
import Kima.Frontend.Tokenizer hiding (Mod)
import qualified Kima.Frontend.Tokenizer as T (Symbol(Mod))
import Kima.Frontend.Types

import Control.Monad.Combinators.Expr
import Data.Bifunctor
import GHC.Exts
import Text.Megaparsec

program :: Parser (ParsedAST 'Module)
program = Program <$> (whitespace *> some topLevel <* eof)

-- Function defintions
topLevel :: Parser (ParsedAST 'TopLevel)
topLevel = funcDef <|> dataDef

funcDef :: Parser (ParsedAST 'TopLevel)
funcDef = reserved RFun *> (
    FuncDefAnn
    <$> identifier
    <*> parens typedArgList
    <*> (symbol Arrow *> typeExpr)
    <*> block)

dataDef :: Parser (ParsedAST 'TopLevel)
dataDef = reserved RData *> (
    DataDefAnn 
    <$> identifier
    <*> braces ((first Accessor <$> typedArg) `sepBy` symbol Comma))
    <?> "Datatype declaration"

typedArgList :: Parser [(ParsedName, TypeExpr)]
typedArgList = typedArg `sepBy` symbol Comma
    <?> "Argument list"

typedArg :: IsString s => Parser (s, TypeExpr)
typedArg = (,) <$> identifier <*> (symbol Colon *> typeExpr)

-- Statements

stmt :: Parser (ParsedAST 'Stmt)
stmt = letStmt <|> varStmt <|> whileStmt <|> ifStmt <|> assignStmt <|> exprStmt 
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
ifStmt = If <$> (IfStmt 
    <$> (reserved RIf *> expr)
    <*> block 
    <*> (reserved RElse *> block))

-- Expressions

expr :: Parser (ParsedAST 'Expr)
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
      ]
    ] <?> "expression"

binary  p f = InfixL  (f <$ p)
prefix  p f = Prefix  (f <$ p)
postfix p f = Postfix (f <$ p)

term :: Parser (ParsedAST 'Expr) 
term = try accessCall <|> baseTerm

-- | A term without calls (Useful for parsing calls)
baseTerm :: Parser (ParsedAST 'Expr)
baseTerm = parens expr
       <|> LiteralE . StringExpr     <$> try string
       <|> LiteralE . FloatExpr      <$> try floatLiteral
       <|> LiteralE . IntExpr        <$> try intLiteral
       <|> LiteralE . BoolExpr       <$> try boolLiteral
       <|>            Identifier     <$> try identifier

argList :: Parser [ParsedAST 'Expr]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls and accesses (a.b)
accessCall :: Parser (ParsedAST 'Expr)
accessCall = do 
    callee <- baseTerm 
    argLists <- some callOrAccess
    return (foldl combiner callee argLists)
    where
        callOrAccess =
            Left  <$> (symbol Dot *> identifier) <|>
            Right <$> parens (expr `sepBy` symbol Comma)

        combiner acc (Left  attr) = Call (Identifier (Accessor attr)) [acc]
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
