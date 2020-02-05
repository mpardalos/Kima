module Kima.Syntax.Tokenizer where

import           Control.Monad
import           Data.Functor
import           Data.Char
import           GHC.Exts

import           Kima.Syntax.Types

import           Text.Megaparsec
import           Text.Megaparsec.Char          as C
                                         hiding ( newline )
import qualified Text.Megaparsec.Char.Lexer    as L

-- Base combinators

whitespace :: Parser ()
whitespace = L.space space1 skipLineComment noBlockComment
  where
    skipLineComment = L.skipLineComment "#"
    noBlockComment  = empty

inlineWhitespace :: Parser ()
inlineWhitespace = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

verbatim :: String -> Parser String
verbatim = lexeme . C.string

parens = between (verbatim "(") (verbatim ")")
brackets = between (verbatim "[") (verbatim "]")
braces = between (verbatim "{") (verbatim "}")

-- Literals

isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = isAlpha c || elem @[] c "$_"

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isIdentifierStartChar c || isNumber c

intLiteral :: Integral a => Parser a
intLiteral = lexeme L.decimal

floatLiteral :: RealFloat a => Parser a
floatLiteral = lexeme $ L.signed inlineWhitespace L.float

boolLiteral :: Parser Bool
boolLiteral = lexeme $ (reserved RTrue $> True) <|> (reserved RFalse $> False)

identifier :: IsString s => Parser s
identifier = do
    idName <- lexeme
        (   (:)
        <$> satisfy isIdentifierStartChar
        <*> takeWhileP Nothing isIdentifierChar
        )
    if idName `elem` reservedWords
        then fail ("\"" ++ idName ++ "\" is a reserved word")
        else return $ fromString idName

string :: IsString s => Parser s
string = lexeme $ char '"' *> (fromString <$> takeWhileP Nothing (/= '"')) <* char '"'

-- Reserved words and symbols

-- | A type that can be parsed as a simple string
class StringToken a where
    -- | The canonical representation of the token
    toString :: a -> String

    -- | The parser for a given token. Defined in terms of 'toString'
    parserFor :: a -> Parser ()
    parserFor t = void (verbatim $ toString t) <?> toString t

data Reserved = RWhile | RFun | RTrue | RFalse | RLet | RVar | RIf | RElse | RData
    deriving (Eq, Enum, Bounded)

-- Reserved words

instance StringToken Reserved where
    toString RFun   = "fun"
    toString RData  = "data"
    toString RTrue  = "True"
    toString RFalse = "False"
    toString RLet   = "let"
    toString RVar   = "var"
    toString RWhile = "while"
    toString RIf    = "if"
    toString RElse  = "else"

reserved :: Reserved -> Parser ()
reserved = parserFor

reservedWords :: [String]
reservedWords = toString <$> ([minBound .. maxBound] :: [Reserved])

-- Symbols

data Symbol = Quote | Bang | Plus | Minus | Star | Slash | Slashslash | Mod
            | Comma | Semicolon | Colon | Equals | Newline | Ellipsis | Arrow
            | GreaterThan | GreaterEqual | LessThan | LessEqual | EqualsEquals
            | BangEquals | Dot | FatArrow

symbol :: Symbol -> Parser ()
symbol = parserFor

instance StringToken Symbol where
    toString Arrow        = "->"
    toString FatArrow     = "=>"
    toString Quote        = "\""
    toString Bang         = "!"
    toString Dot          = "."
    toString Plus         = "+"
    toString Minus        = "-"
    toString Star         = "*"
    toString Slash        = "/"
    toString Slashslash   = "//"
    toString Mod          = "%"
    toString Comma        = ","
    toString Semicolon    = ";"
    toString Colon        = ":"
    toString Equals       = "="
    toString Newline      = "\n"
    toString Ellipsis     = "..."
    toString GreaterThan  = ">"
    toString GreaterEqual = ">="
    toString LessThan     = "<"
    toString LessEqual    = "<="
    toString EqualsEquals = "=="
    toString BangEquals   = "!="

stmtEnd = parserFor Semicolon <|> parserFor Newline
