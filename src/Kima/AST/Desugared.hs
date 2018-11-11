module Kima.AST.Desugared where

import Kima.AST.Expression
import Kima.AST.Common as Common

newtype Program = Program [Kima.AST.Desugared.FuncDef]
type FuncDef = Common.FuncDef () Stmt

data Expr = LiteralExpr Literal 
          | Identifier Name
          | FuncExpr (NamedSignature ()) Stmt
          | Call Expr [Expr]

data Stmt = BlockStmt [Stmt]
          | Assign Name Expr
          | WhileStmt Expr Stmt
          | ExprStmt Expr 
          | IfStmt Expr Stmt Stmt
