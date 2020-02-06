module Kima.AST.Effects
    ( Effect
    , EffectName
    , noEffect
    , ioEffect
    , fromEffectNames
    , isSubEffect
    )
where

import Data.Text.Prettyprint.Doc
import GHC.Exts

newtype Effect = EffectNames [EffectName]
    deriving (Show, Eq, Ord, IsList, Semigroup, Monoid)

type EffectName = String

noEffect :: Effect
noEffect = EffectNames []

ioEffect :: Effect
ioEffect = EffectNames ["IO"]

fromEffectNames :: [EffectName] -> Effect
fromEffectNames = EffectNames

isSubEffect :: Effect -> Effect -> Bool
isSubEffect (EffectNames subEff) (EffectNames superEff) =
    and ((`elem` superEff) <$> subEff)

instance Pretty Effect where
    pretty (EffectNames [eff]) = pretty eff
    pretty (EffectNames effects) =
        encloseSep lbrace rbrace comma (pretty <$> effects)
