{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Kima.Builtins.Types where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

import Kima.AST
import Kima.Builtins.Values
import Kima.Types.TypeCtx

baseBindings :: Map (Identifier 'NoAnnotation) [KType]
baseBindings = Map.foldlWithKey combine Map.empty baseEnv
  where
    combine typeCtx name _ =
        Map.insertWith (<>) (deTypeAnnotate name) [nameType name] typeCtx

baseTypeBindings :: Map String KType
baseTypeBindings=
    [ ("String", KString)
    , ("Unit", KUnit)
    , ("Int", KInt)
    , ("Float", KFloat)
    , ("Bool", KBool)
    ]

ioEffect :: KEffect
ioEffect = KEffect (Just "IO")
    [ printStringOperation
    , printIntOperation
    , printFloatOperation
    , printBoolOperation
    , printUnitOperation
    , inputOperation
    ]

baseTypeCtx :: TypeCtx
baseTypeCtx = TypeCtx
    { typeBindings = baseTypeBindings
    , effectBindings = [("IO", ioEffect)]
    , bindings = Binding Constant . Set.fromList <$> baseBindings
    , activeEffect = PureEffect
    }
