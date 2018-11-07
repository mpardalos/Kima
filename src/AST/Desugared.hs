module AST.Desugared where

import AST.Expression
import AST.Statement
import Control.Newtype.Generics
import Data.Comp
import Data.Comp.Show()
import GHC.Generics hiding ((:+:))

type DesugaredExprF s = (Literal :+: Identifier :+: FuncExpr s :+: Call)
type DesugaredExprTerm = Term (DesugaredExprF DesugaredStmt)
newtype DesugaredExpr = DesugaredExpr DesugaredExprTerm deriving (Show, Generic)

type DesugaredStmtF e = (BlockStmt :+: SimpleAssignment e :+: WhileLoop e :+: ExprStmt e :+: IfStmt e)
type DesugaredStmtTerm = Term (DesugaredStmtF DesugaredExpr)
newtype DesugaredStmt = DesugaredStmt DesugaredStmtTerm deriving (Show, Generic)

instance Newtype DesugaredExpr
instance Newtype DesugaredStmt
