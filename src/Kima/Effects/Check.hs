module Kima.Effects.Check where

import Kima.AST

checkEffects :: AST p Typed -> Bool
checkEffects ast = _

supports :: EffectSpec -> EffectSpec -> Bool
supports (EffectList e1) (EffectList e2) = _
