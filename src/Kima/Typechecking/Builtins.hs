module Kima.Typechecking.Builtins where

import Data.Map

import Kima.AST
import Kima.Typechecking.Types
import Kima.KimaTypes

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