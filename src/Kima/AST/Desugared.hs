module Kima.AST.Desugared where

import GHC.Generics
import Control.Newtype.Generics

import Kima.AST.Expression
import Kima.AST.Common as Common

newtype Program = Program [Kima.AST.Desugared.FuncDef] deriving (Show, Generic)
type FuncDef = Common.FuncDef () Stmt

data Expr = LiteralExpr Literal 
          | Identifier Name
          | FuncExpr (NamedSignature ()) Stmt
          | Call Expr [Expr]
    deriving Show

data Stmt = BlockStmt [Stmt]
          | AssignStmt Name Expr
          | WhileStmt Expr Stmt
          | ExprStmt Expr 
          | IfStmt Expr Stmt Stmt
    deriving Show

instance Newtype Program