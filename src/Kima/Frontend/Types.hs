module Kima.Frontend.Types where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String
type ParseError = Text.Megaparsec.ParseError Char Void