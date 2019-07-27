module Kima.Frontend.Types where

import Data.Void
import Kima.AST
import Text.Megaparsec

type ParsedASTTag tag
    = ( HasSugar tag
      , FreeAnnotation tag ~ TypeExpr
      , NameAnnotation tag ~ 'NoAnnotation
      )
type Parser = Parsec Void String
