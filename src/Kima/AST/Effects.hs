module Kima.AST.Effects where

import GHC.Exts
import Data.Text.Prettyprint.Doc

type EffectName = String

data EffectExpr = EffectList [EffectName]
    deriving (Show, Eq)

instance IsList EffectExpr where
    type Item EffectExpr = EffectName

    fromList = EffectList
    toList (EffectList effs) = effs

instance IsString EffectExpr where
    fromString s = EffectList [s]

instance Pretty EffectExpr where
    pretty (EffectList effs) = prettyList effs
