module Kima.Frontend.Tokenizer where

import Control.Monad
import Data.Char

import Kima.AST hiding (Mod)
import Kima.Frontend.Types

import Text.Megaparsec
import Text.Megaparsec.Char          as C hiding ( newline )
import qualified Text.Megaparsec.Char.Lexer    as L

-- Base combinators

whitespace :: Parser ()
whitespace = L.space space1 skipLineComment empty
    where skipLineComment = L.skipLineComment "#"

inlineWhitespace :: Parser ()
inlineWhitespace = L.space space1 empty empty

-- | These consume whitespace before them
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

intLiteral :: Parser Integer
intLiteral = L.decimal

floatLiteral :: Parser Double
floatLiteral = L.signed inlineWhitespace L.float

identifier :: Parser Name
identifier = do
    idName <- lexeme ((:) <$> satisfy isIdentifierStartChar <*> takeWhileP Nothing isIdentifierChar)
    if idName `elem` reservedWords
        then fail ("\"" ++ idName ++ "\" is a reserved word")
        else return $ Name idName

string :: Parser String
string = char '"' *> takeWhileP Nothing (/= '"') <* char '"'

-- Reserved words and symbols

-- | A type that can be parsed as a simple string
class StringToken a where
    -- | The canonical representation of the token
    toString :: a -> String

    -- | The parser for a given token. Defined in terms of 'toString'
    parserFor :: a -> Parser ()
    parserFor t = void (verbatim $ toString t) <?> toString t

data Reserved = RWhile | RFun | RTrue | RFalse | RLet | RVar | RIf
    deriving (Eq, Enum, Bounded)

-- Reserved words

instance StringToken Reserved where
    toString RFun   = "fun"
    toString RTrue  = "True"
    toString RFalse = "False"
    toString RLet   = "let"
    toString RVar   = "var"
    toString RWhile = "while"
    toString RIf    = "if"

reserved :: Reserved -> Parser ()
reserved = parserFor

reservedWords :: [String]
reservedWords = toString <$> ([minBound .. maxBound] :: [Reserved])

-- Symbols

data Symbol = Quote | Bang | Plus | Minus | Star | Slash | Slashslash | Mod 
            | Comma | Semicolon | Colon | Equals | Newline | Ellipsis | Arrow

symbol :: Symbol -> Parser ()
symbol = parserFor

instance StringToken Symbol where
    toString Quote      = "\""
    toString Bang       = "!"
    toString Plus       = "+"
    toString Minus      = "-"
    toString Star       = "*"
    toString Slash      = "/"
    toString Slashslash = "//"
    toString Mod        = "%"
    toString Comma      = ","
    toString Semicolon  = ";"
    toString Colon      = ":"
    toString Equals     = "="
    toString Newline    = "\n"
    toString Ellipsis   = "..."
    toString Arrow      = "->"

stmtEnd = parserFor Semicolon <|> parserFor Newline
