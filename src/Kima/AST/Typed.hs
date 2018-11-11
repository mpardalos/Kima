module Kima.AST.Typed where

import Kima.AST.Common as Common
import Kima.AST.Expression
import Kima.KimaTypes

newtype Program = Program [Kima.AST.Typed.FuncDef] deriving Show
type FuncDef = Common.FuncDef KType Stmt

data Stmt = BlockStmt [Stmt]
          | WhileStmt Expr Stmt
          | ExprStmt Expr
          | IfStmt Expr Stmt Stmt
          | AssignStmt Name Expr
          | VarStmt Name KType Expr
          | LetStmt Name KType Expr
    deriving Show

data Expr = Identifier Name KType
          | FuncExpr (NamedSignature KType) Stmt
          | Call Expr [Expr] KType
          | LiteralExpr Literal KType
          | BinExpr (Binary Expr) KType
          | UnaryExpr (Unary Expr) KType
    deriving Show