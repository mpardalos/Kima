module AST.Statement where

import AST.Common

import Data.Comp.Sum
import Data.Comp.Derive

type StmtF e = BlockStmt :+: QualifiedAssignment e :+: WhileLoop e :+: ExprStmt e
type DesugaredStmtF e = BlockStmt :+: SimpleAssignment e :+: WhileLoop e :+: ExprStmt e

data    SimpleAssignment e s    = SimpleAssignStmt Name e
data    QualifiedAssignment e s = LetStmt Name TypeExpr e
                                | VarStmt Name TypeExpr e
                                | AssignStmt Name e
data    WhileLoop e s           = WhileStmt e s
newtype ExprStmt e s            = ExprStmt e
newtype BlockStmt s             = BlockStmt [s] 


deriving instance Functor BlockStmt
deriving instance Functor (SimpleAssignment e)
deriving instance Functor (QualifiedAssignment e)
deriving instance Functor (WhileLoop e)
deriving instance Functor (ExprStmt e)

$(derive [makeShowF, makeFoldable, makeTraversable, smartAConstructors, smartConstructors]
 [''BlockStmt, ''SimpleAssignment, ''QualifiedAssignment, ''WhileLoop, ''ExprStmt])