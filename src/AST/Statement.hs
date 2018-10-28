module AST.Statement where

import Data.Comp.Sum
import Data.Comp.Derive

import AST.Common

type StmtF e = QualifiedAssignment e :+: WhileLoop e :+: ExprStmt e
type DesugaredStmtF e = SimpleAssignment e :+: WhileLoop e :+: ExprStmt e

data    SimpleAssignment e s    = SimpleAssignStmt Name e
data    QualifiedAssignment e s = LetStmt Name TypeExpr e
                                | VarStmt Name TypeExpr e
                                | AssignStmt Name e
data    WhileLoop e s           = WhileStmt e (Block s)
newtype ExprStmt e s            = ExprStmt e

deriving instance Functor (SimpleAssignment e)
deriving instance Functor (QualifiedAssignment e)
deriving instance Functor (WhileLoop e)
deriving instance Functor (ExprStmt e)

$(derive [makeShowF, makeFoldable, makeTraversable, smartAConstructors, smartConstructors]
 [''SimpleAssignment, ''QualifiedAssignment, ''WhileLoop, ''ExprStmt])