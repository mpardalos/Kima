module Kima.Frontend(
    module Kima.Frontend.Types,
    program, funcDef, block, stmt, expr,
    parseProgram, parseErrorPretty, errorBundlePretty, runParser
) where

import Kima.Frontend.Parser
import Kima.Frontend.Types 
import Text.Megaparsec

-- | Parse a string as a Kima program and return either a ParseError that was
-- | encoutered or the resulting AST
parseProgram = runParser program 
