module Kima.Frontend.Parser where

import Prelude hiding (mod)

import Kima.AST
import Kima.Frontend.Tokenizer hiding (Mod)
import qualified Kima.Frontend.Tokenizer as T (Symbol(Mod))
import Kima.Frontend.Types

import Control.Monad.Combinators.Expr
import GHC.Exts
import Text.Megaparsec

program :: ParsedASTTag tag => Parser (AST 'Module tag)
program = Program <$> (whitespace *> some topLevel <* eof)

-- Function defintions
topLevel :: ParsedASTTag tag => Parser (AST 'TopLevel tag)
topLevel = funcDef <|> dataDef

funcDef :: ParsedASTTag tag => Parser (AST 'TopLevel tag)
funcDef = reserved RFun *> (
    FuncDef
    <$> identifier
    <*> parens typedArgList
    <*> (symbol Arrow *> typeExpr)
    <*> block)

dataDef :: ParsedASTTag tag => Parser (AST 'TopLevel tag)
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

stmt :: ParsedASTTag tag => Parser (AST 'Stmt tag)
stmt = letStmt <|> varStmt <|> whileStmt <|> ifStmt <|> assignStmt <|> exprStmt
    <?> "statement"

block :: ParsedASTTag tag => Parser (AST 'Stmt tag)
block = Block <$> braces (stmt `sepEndBy` stmtEnd)
    <?> "Block"

letStmt :: ParsedASTTag tag => Parser (AST 'Stmt tag)
letStmt = Let
    <$> (reserved RLet *> identifier)
    <*> (symbol Colon *> typeExpr)
    <*> (symbol Equals *> expr)
    <?> "let statement"

varStmt :: ParsedASTTag tag => Parser (AST 'Stmt tag)
varStmt = Var
    <$> (reserved RVar *> identifier)
    <*> (symbol Colon *> typeExpr)
    <*> (symbol Equals *> expr)
    <?> "var statement"

assignStmt :: ParsedASTTag tag => Parser (AST 'Stmt tag)
assignStmt = try (Assign
    <$> writeAccess
    <*> (symbol Equals *> expr))
    <?> "assignment"

writeAccess :: IsString s => Parser (WriteAccess s)
writeAccess = label "accessor" $ do
    base <- identifier
    fields <- option [] (symbol Dot *> identifier `sepBy1` symbol Dot)
    return (WriteAccess base fields)

whileStmt :: ParsedASTTag tag => Parser (AST 'Stmt tag)
whileStmt = While <$> (WhileStmt
    <$> (reserved RWhile *> expr)
    <*> block)
    <?> "while statement"

exprStmt :: ParsedASTTag tag => Parser (AST 'Stmt tag)
exprStmt = ExprStmt <$> expr
    <?> "expression statement"

ifStmt :: ParsedASTTag tag => Parser (AST 'Stmt tag)
ifStmt = If <$> (IfStmt
    <$> (reserved RIf *> expr)
    <*> block
    <*> (reserved RElse *> block))

-- Expressions

expr :: ParsedASTTag tag => Parser (AST 'Expr tag)
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

term :: ParsedASTTag tag => Parser (AST 'Expr tag)
term = try accessCall <|> funcExpr <|> baseTerm

funcExpr :: ParsedASTTag tag => Parser (AST 'Expr tag)
funcExpr = reserved RFun *> (
    FuncExpr
    <$> parens typedArgList
    <*> (symbol Arrow *> typeExpr)
    <*> block)

-- | A term without calls (Useful for parsing calls)
baseTerm :: ParsedASTTag tag => Parser (AST 'Expr tag)
baseTerm = parens expr
       <|> LiteralE    . StringExpr <$> try string
       <|> LiteralE    . FloatExpr  <$> try floatLiteral
       <|> LiteralE    . IntExpr    <$> try intLiteral
       <|> LiteralE    . BoolExpr   <$> try boolLiteral
       <|> IdentifierE . Identifier <$> try identifier

argList :: ParsedASTTag tag => Parser [AST 'Expr tag]
argList = parens (expr `sepBy` symbol Comma)

-- | Parse a series of nested calls and accesses (a.b)
accessCall :: ParsedASTTag tag => Parser (AST 'Expr tag)
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
