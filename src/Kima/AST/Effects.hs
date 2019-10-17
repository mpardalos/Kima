module Kima.AST.Effects where

import GHC.Exts
import Data.Text.Prettyprint.Doc
import Data.Set

type EffectName = String
newtype EffectExpr = EffectNames [EffectName]
    deriving (Show, Eq)

data BuiltinEffect = KimaIO
    deriving (Eq, Ord)

data Effect
    = BuiltinEffect BuiltinEffect
    | UserEffect String
    deriving (Eq, Ord)

newtype EffectSpec = EffectSpec (Set Effect)
    deriving IsList

instance IsList EffectExpr where
    type Item EffectExpr = EffectName

    fromList = EffectNames
    toList (EffectNames effs) = effs

instance IsString EffectExpr where
    fromString s = EffectNames [s]

instance Pretty EffectExpr where
    pretty (EffectNames effs) = prettyList effs
