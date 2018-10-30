module AST (
    module E,
    Stmt(..), Expr(..), StmtTerm, ExprTerm,
    DesugaredStmt(..), DesugaredExpr(..), DesugaredStmtTerm, DesugaredExprTerm,
    desugarExpr, desugarStmt, desugarFuncDef
) where

import AST.Common as E
import AST.Expression as E
import AST.Statement as E

import Data.Bifunctor

import Data.Comp.Desugar
import Data.Comp.Ops
import Data.Comp.Show()
import Data.Comp.Sum
import Data.Comp.Term

import Data.Newtype

type ExprTerm = Term (ExprF Stmt)
type StmtTerm = Term (StmtF Expr)

type DesugaredExprTerm = Term (DesugaredExprF DesugaredStmt)
type DesugaredStmtTerm = Term (DesugaredStmtF DesugaredExpr)

newtype Stmt = Stmt StmtTerm deriving Show
newtype Expr = Expr ExprTerm deriving Show

newtype DesugaredStmt = DesugaredStmt DesugaredStmtTerm deriving Show
newtype DesugaredExpr = DesugaredExpr DesugaredExprTerm deriving Show


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

instance (Functor g, (FuncExpr DesugaredStmt) :<: g) => Desugar (FuncExpr Stmt) g where
    desugHom (FuncExpr sig body) = iFuncExpr sig (desugarStmt body)

instance (Functor g, (SimpleAssignment DesugaredExpr) :<: g) => Desugar (QualifiedAssignment Expr) g where
    desugHom (LetStmt name _ expr)  = iSimpleAssignStmt name (desugarExpr expr)
    desugHom (VarStmt name _ expr)  = iSimpleAssignStmt name (desugarExpr expr)
    desugHom (AssignStmt name expr) = iSimpleAssignStmt name (desugarExpr expr)

instance {-# OVERLAPPABLE #-} (Bifunctor f, Functor g, Functor (f Expr), Functor (f DesugaredExpr), (f DesugaredExpr) :<: g) => Desugar (f Expr) g where
    desugHom stmt = deepInject (simpCxt desugaredStmt)
        where
            desugaredStmt = desugarExpr `first` stmt

desugarExpr :: Expr -> DesugaredExpr
desugarExpr (Expr expr) = DesugaredExpr (desugar expr)

desugarStmt :: Stmt -> DesugaredStmt
desugarStmt (Stmt stmt) = DesugaredStmt (desugar stmt)

desugarFuncDef :: FuncDef Stmt -> FuncDef DesugaredStmt
desugarFuncDef FuncDef { name, signature, body} = FuncDef name signature (desugarStmt body)