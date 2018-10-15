module Frontend(
    module Frontend.Types,
    program, funcDef, block, stmt, expr,
    parseProgram, parseErrorPretty, runParser
) where

import Frontend.Types 
import Frontend.Parser
import Text.Megaparsec

-- | Parse a string as a Kima program and return either a ParseError that was
-- | encoutered or the resulting AST
parseProgram = runParser program 
