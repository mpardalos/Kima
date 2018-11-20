module Kima.Typechecking.Builtins where

import Data.Map

import Kima.AST
import Kima.Typechecking.Types
import Kima.KimaTypes

builtinTypes :: Map ParsedName (KType o)
builtinTypes = fromList 
    [ ("String", KString)
    , ("Unit"  , KUnit)
    , ("Bool"  , KBool)
    , ("Int"   , KInt)
    , ("Float" , KFloat)
    ]


builtinBindings :: Map ParsedName (Binding (KType 'Overload))
builtinBindings = fromList 
    [ ("print", Constant $ KFuncOv
        [ [KString] $-> KUnit 
        , [KUnit] $-> KUnit 
        , [KBool] $-> KUnit 
        , [KInt] $-> KUnit 
        , [KFloat] $-> KUnit 
        ])
    ]

baseCtx = TypeCtx builtinTypes builtinBindings