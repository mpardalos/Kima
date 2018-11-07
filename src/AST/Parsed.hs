module AST.Parsed where

import AST.Common
import AST.Desugared
import AST.Expression
import AST.Statement

import Control.Newtype.Generics

import Data.Bifunctor
import Data.Comp
import Data.Comp.Desugar
import Data.Comp.Show()
import GHC.Generics hiding ((:+:))

type StmtF e = (BlockStmt :+: QualifiedAssignment e :+: WhileLoop e :+: ExprStmt e :+: IfStmt e)
type ExprF s = (Literal :+: Identifier :+: FuncExpr s :+: Call :+: BinExpr :+: UnaryExpr)

type ExprTerm = Term (ExprF Stmt)
type StmtTerm = Term (StmtF Expr)

newtype Stmt = Stmt StmtTerm deriving (Show, Generic)
newtype Expr = Expr ExprTerm deriving (Show, Generic)

instance Newtype Stmt
instance Newtype Expr

instance (Functor g, (SimpleAssignment DesugaredExpr) :<: g) => Desugar (QualifiedAssignment Expr) g where
    desugHom (LetStmt name _ expr)  = iSimpleAssignStmt name (desugarExpr expr)
    desugHom (VarStmt name _ expr)  = iSimpleAssignStmt name (desugarExpr expr)
    desugHom (AssignStmt name expr) = iSimpleAssignStmt name (desugarExpr expr)

instance {-# OVERLAPPABLE #-} (Bifunctor f, Functor g, Functor (f Expr), Functor (f DesugaredExpr), (f DesugaredExpr) :<: g) => Desugar (f Expr) g where
    desugHom stmt = deepInject (simpCxt desugaredStmt)
        where
            desugaredStmt = desugarExpr `first` stmt

instance {-# OVERLAPPABLE #-} (Bifunctor f, Functor g, Functor (f Stmt), Functor (f DesugaredStmt), (f DesugaredStmt) :<: g) => Desugar (f Stmt) g where
    desugHom expr = deepInject (simpCxt desugaredExpr)
        where
            desugaredExpr = desugarStmt `first` expr

desugarExpr :: Expr -> DesugaredExpr
desugarExpr (Expr expr) = DesugaredExpr (desugar expr)

desugarStmt :: Stmt -> DesugaredStmt
desugarStmt (Stmt stmt) = DesugaredStmt (desugar stmt)

desugarFuncDef :: FuncDef Stmt -> FuncDef DesugaredStmt
desugarFuncDef FuncDef { name, signature, body} = FuncDef name signature (desugarStmt body)