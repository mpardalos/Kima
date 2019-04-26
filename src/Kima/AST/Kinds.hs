-- | Datakinds used to classify various parts of the AST.

module Kima.AST.Kinds where

import Data.Kind

data Sugar = Sugar | NoSugar
data ASTPart = Expr | Stmt | TopLevel | Module
data HasAnnotation = NoAnnotation | Annotation Type

type family AnnotationConstraint (f :: k -> Constraint) (x :: HasAnnotation) :: Constraint where
    AnnotationConstraint f 'NoAnnotation   = ()
    AnnotationConstraint f ('Annotation a) = f a