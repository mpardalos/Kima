module AST (
    module E,
    Stmt(Stmt), Expr(Expr), 
    DesugaredStmt(DesugaredStmt), DesugaredExpr(DesugaredExpr)
) where

import AST.Common as E
import AST.Expression as E
import AST.Statement as E

import Data.Comp.Term

newtype Stmt = Stmt (Term (StmtF Expr))
newtype Expr = Expr (Term (ExprF Stmt))

newtype DesugaredStmt = DesugaredStmt (Term (DesugaredStmtF DesugaredExpr))
newtype DesugaredExpr = DesugaredExpr (Term (DesugaredExprF DesugaredStmt))