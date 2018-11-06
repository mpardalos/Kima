module AST (
    module E,
    Stmt(..), Expr(..), StmtTerm, ExprTerm,
    DesugaredStmt(..), DesugaredExpr(..), DesugaredStmtTerm, DesugaredExprTerm,
    desugarExpr, desugarStmt, desugarFuncDef
) where

import AST.Common as E
import AST.Expression as E
import AST.Statement as E

import Control.Newtype.Generics

import Data.Bifunctor

import Data.Comp.Desugar
import Data.Comp.Ops
import Data.Comp.Show()
import Data.Comp.Sum
import Data.Comp.Term

import GHC.Generics

type ExprTerm = Term (ExprF Stmt)
type StmtTerm = Term (StmtF Expr)

type DesugaredExprTerm = Term (DesugaredExprF DesugaredStmt)
type DesugaredStmtTerm = Term (DesugaredStmtF DesugaredExpr)

newtype Stmt = Stmt StmtTerm deriving (Show, Generic)
newtype Expr = Expr ExprTerm deriving (Show, Generic)
newtype DesugaredStmt = DesugaredStmt DesugaredStmtTerm deriving (Show, Generic)
newtype DesugaredExpr = DesugaredExpr DesugaredExprTerm deriving (Show, Generic)

instance Newtype Stmt
instance Newtype Expr
instance Newtype DesugaredStmt
instance Newtype DesugaredExpr

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