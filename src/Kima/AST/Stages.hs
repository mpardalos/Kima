-- | Stage types for the AST

module Kima.AST.Stages where

import Kima.AST.Kinds
import Kima.AST.Types
import Kima.AST.Effects

-- | Initial stage at parsing
data Parsed
instance ASTTag Parsed where
    type TagSugar Parsed = 'Sugar
    type NameAnnotation Parsed = 'NoAnnotation
    type FreeAnnotation Parsed = Maybe TypeExpr
    type EffectType Parsed = Maybe EffectExpr


-- | Removing syntactic sugar
data Desugared
instance ASTTag Desugared where
    type TagSugar Desugared = 'NoSugar
    type NameAnnotation Desugared = 'NoAnnotation
    type FreeAnnotation Desugared = Maybe TypeExpr
    type EffectType Desugared = Maybe EffectExpr

-- | Resolve type expressions to their representations
data TypeAnnotated
instance ASTTag TypeAnnotated where
    type TagSugar TypeAnnotated = 'NoSugar
    type NameAnnotation TypeAnnotated = 'NoAnnotation
    type FreeAnnotation TypeAnnotated = Maybe KType
    type EffectType TypeAnnotated = Maybe EffectExpr

-- | Every identifier is annotated with its type.
data Typed
instance ASTTag Typed where
    type TagSugar Typed = 'NoSugar
    type NameAnnotation Typed = 'Annotation KType
    type FreeAnnotation Typed = KType
    type EffectType Typed = Maybe EffectExpr

-- | The interpreter executes the typed AST
type Runtime = Typed
