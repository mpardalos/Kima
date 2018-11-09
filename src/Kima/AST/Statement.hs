module Kima.AST.Statement where

import Kima.AST.Common

import Data.Bifunctor
import Data.Comp.Derive

newtype BlockStmt s             = BlockStmt [s] 
newtype ExprStmt e s            = ExprStmt e
data    IfStmt e s              = IfStmt e s s
data    QualifiedAssignment e s = LetStmt Name TypeExpr e 
                                | VarStmt Name TypeExpr e 
                                | AssignStmt Name e
data    SimpleAssignment e s    = SimpleAssignStmt Name e
data    WhileLoop e s           = WhileStmt e s

deriving instance Functor BlockStmt
deriving instance Functor (ExprStmt e)
deriving instance Functor (IfStmt e)
deriving instance Functor (QualifiedAssignment e)
deriving instance Functor (SimpleAssignment e)
deriving instance Functor (WhileLoop e)

instance Bifunctor SimpleAssignment where
    bimap f _ (SimpleAssignStmt name expr) = SimpleAssignStmt name (f expr)
instance Bifunctor QualifiedAssignment where
    bimap f _ (LetStmt n t expr) = LetStmt n t (f expr)
    bimap f _ (VarStmt n t expr) = VarStmt n t (f expr)
    bimap f _ (AssignStmt n expr) = AssignStmt n (f expr)
instance Bifunctor WhileLoop where
    bimap f g (WhileStmt cond body) = WhileStmt (f cond) (g body)
instance Bifunctor ExprStmt where
    bimap f _ (ExprStmt expr) = ExprStmt (f expr)
instance Bifunctor IfStmt where
    bimap f g (IfStmt cond ifBlk elseBlk) = IfStmt (f cond) (g ifBlk) (g elseBlk)

$(derive [makeShowF, makeFoldable, makeTraversable, smartAConstructors, smartConstructors]
 [''BlockStmt, ''SimpleAssignment, ''QualifiedAssignment, ''WhileLoop, ''ExprStmt, ''IfStmt])