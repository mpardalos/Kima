module Kima.Syntax.Types where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String
