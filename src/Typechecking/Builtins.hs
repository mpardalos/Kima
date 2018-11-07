module Typechecking.Builtins where

import AST
import Typechecking.Types
import Data.Map
import KimaTypes

builtinTypes :: Map Name KType
builtinTypes = fromList 
    [ ("String", KString)
    , ("Unit"  , KUnit)
    , ("Bool"  , KBool)
    , ("Int"   , KInt)
    , ("Float" , KFloat)
    ]


builtinBindings :: Map Name TypeBinding
builtinBindings = fromList 
    [ ("print", Constant $ KFunc 
        [ [KString] $-> KUnit 
        , [KUnit] $-> KUnit 
        , [KBool] $-> KUnit 
        , [KInt] $-> KUnit 
        , [KFloat] $-> KUnit 
        ])
    ]

baseCtx = TypeCtx builtinTypes builtinBindings