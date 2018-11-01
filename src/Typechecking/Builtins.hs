module Typechecking.Builtins where

import AST
import Typechecking.Types
import Data.Map

builtinTypes :: Map Name KType
builtinTypes = fromList 
    [ ("String", KString)
    , ("Unit"  , KUnit)
    , ("Bool"  , KBool)
    , ("Int"   , KInt)
    , ("Float" , KFloat)
    ]

baseCtx = TypeCtx builtinTypes mempty