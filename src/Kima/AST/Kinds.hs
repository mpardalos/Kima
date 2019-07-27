-- | Datakinds used to classify various parts of the AST.

module Kima.AST.Kinds where

import Data.Kind

data Sugar = Sugar | NoSugar
data ASTPart = Expr | Stmt | TopLevel | Module
data HasAnnotation = NoAnnotation | Annotation Type

type family AnnotationConstraint (f :: k -> Constraint) (x :: HasAnnotation) :: Constraint where
    AnnotationConstraint f 'NoAnnotation   = ()
    AnnotationConstraint f ('Annotation a) = f a

class ASTTag s where
    type Part s :: ASTPart

    type TagSugar s :: Sugar
    type NameAnnotation s :: HasAnnotation
    type FreeAnnotation s :: Type

type HasSugar s = TagSugar s ~ 'Sugar

-- | Two tags are equal in every way except their AST part
type TagEqual t1 t2 = ( TagSugar t1 ~ TagSugar t2
                      , NameAnnotation t1 ~ NameAnnotation t2
                      , FreeAnnotation t1 ~ FreeAnnotation t2)
