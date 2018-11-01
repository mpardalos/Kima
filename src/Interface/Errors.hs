module Interface.Errors where

import Frontend
import Typechecking
import Interpreter

data KimaError = ParseError ParseError 
               | TypeError TypeError
               | RuntimeError RuntimeError
    deriving Show