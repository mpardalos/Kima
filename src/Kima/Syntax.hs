module Kima.Syntax(
    module Kima.Syntax.Types,
    program, funcDef, block, stmt, expr,
    parseProgram, parseErrorPretty, errorBundlePretty, runParser
) where

import Data.Void
import Kima.AST
import Kima.Syntax.Parser
import Kima.Syntax.Types
import Text.Megaparsec

-- | Parse a string as a Kima program and return either a ParseError that was
-- | encoutered or the resulting AST
parseProgram
    :: String
    -> String
    -> Either (ParseErrorBundle String Void) (AST 'Module Parsed)
parseProgram = runParser program
