-- | Datakinds used to classify various parts of the AST.

module Kima.AST.Kinds where

import Data.Kind

data Sugar = Sugar | NoSugar
data HasAnnotation = NoAnnotation | Annotation Type

type family AnnotationConstraint (f :: k -> Constraint) (x :: HasAnnotation) :: Constraint where
    AnnotationConstraint f 'NoAnnotation   = ()
    AnnotationConstraint f ('Annotation a) = f a

class ASTTag s where
    type TagSugar s :: Sugar
    type NameAnnotation s :: HasAnnotation
    type FreeAnnotation s :: Type
    type EffectType s :: Type

type HasSugar s = TagSugar s ~ 'Sugar
