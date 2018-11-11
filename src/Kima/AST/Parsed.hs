module Kima.AST.Parsed where

import Kima.AST.Common as Common
import Kima.AST.Expression

newtype Program = Program [Kima.AST.Parsed.FuncDef] deriving Show
type FuncDef = Common.FuncDef TypeExpr Stmt

data Stmt = BlockStmt [Stmt] 
          | WhileStmt Expr Stmt
          | ExprStmt Expr 
          | IfStmt Expr Stmt Stmt
          | AssignStmt Name Expr
          | VarStmt Name TypeExpr Expr
          | LetStmt Name TypeExpr Expr          
    deriving Show

data Expr = Identifier Name
          | FuncExpr (NamedSignature TypeExpr) Stmt
          | Call Expr [Expr]

          | LiteralExpr Literal
          | BinExpr (Binary Expr)
          | UnaryExpr (Unary Expr)
    deriving Show