module AST (
    module E,
    Stmt(..), Expr(..), StmtTerm, ExprTerm,
    DesugaredStmt(..), DesugaredExpr(..), DesugaredStmtTerm, DesugaredExprTerm
) where

import AST.Common as E
import AST.Expression as E
import AST.Statement as E

import Data.Comp.Term
import Data.Newtype

type ExprTerm = Term (ExprF Stmt)
type StmtTerm = Term (StmtF Expr)

type DesugaredExprTerm = Term (DesugaredExprF DesugaredStmt)
type DesugaredStmtTerm = Term (DesugaredStmtF DesugaredExpr)

newtype Stmt = Stmt StmtTerm
newtype Expr = Expr ExprTerm

newtype DesugaredStmt = DesugaredStmt DesugaredStmtTerm
newtype DesugaredExpr = DesugaredExpr DesugaredExprTerm

instance Newtype Stmt StmtTerm where
    wrap = Stmt
    unwrap (Stmt s) = s

instance Newtype Expr ExprTerm where
    wrap = Expr
    unwrap (Expr e) = e

instance Newtype DesugaredStmt DesugaredStmtTerm where
    wrap = DesugaredStmt
    unwrap (DesugaredStmt s) = s

instance Newtype DesugaredExpr DesugaredExprTerm where
    wrap = DesugaredExpr
    unwrap (DesugaredExpr e) = e