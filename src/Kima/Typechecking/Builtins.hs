{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.Builtins where

import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.Types

(-@>) = (,)

baseCtx :: TypeCtx
baseCtx =
    [ Builtin PrintFunc
        -@> [ KFunc ([KString] $-> KUnit)
            , KFunc ([KInt] $-> KUnit)
            , KFunc ([KFloat] $-> KUnit)
            , KFunc ([KBool] $-> KUnit)
            ]
    , Builtin AddOp
        -@> [ KFunc ([KString, KString] $-> KString)
            , KFunc ([KInt, KInt] $-> KInt)
            , KFunc ([KInt, KFloat] $-> KFloat)
            , KFunc ([KFloat, KInt] $-> KFloat)
            , KFunc ([KFloat, KFloat] $-> KFloat)
            ]
    , Builtin SubOp
        -@> [ KFunc ([KInt, KInt] $-> KInt)
            , KFunc ([KInt, KFloat] $-> KFloat)
            , KFunc ([KFloat, KInt] $-> KFloat)
            , KFunc ([KFloat, KFloat] $-> KFloat)
            ]
    , Builtin MulOp
        -@> [ KFunc ([KInt, KInt] $-> KInt)
            , KFunc ([KInt, KFloat] $-> KFloat)
            , KFunc ([KFloat, KInt] $-> KFloat)
            , KFunc ([KFloat, KFloat] $-> KFloat)
            ]
    , Builtin DivOp
        -@> [ KFunc ([KInt, KInt] $-> KInt)
            , KFunc ([KInt, KFloat] $-> KFloat)
            , KFunc ([KFloat, KInt] $-> KFloat)
            , KFunc ([KFloat, KFloat] $-> KFloat)
            ]
    , Builtin ModOp -@> [KFunc ([KInt, KInt] $-> KInt)]
    , Builtin InvertOp -@> [KFunc ([KBool] $-> KBool)]
    , Builtin NegateOp
        -@> [KFunc ([KInt] $-> KInt), KFunc ([KFloat] $-> KFloat)]
    ]
