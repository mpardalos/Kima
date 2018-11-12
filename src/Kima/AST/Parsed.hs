module Kima.AST.Parsed where

import GHC.Generics
import Control.Newtype.Generics

import Kima.AST.Common hiding (NamedSignature, FuncDef)
import qualified Kima.AST.Common as Common
import Kima.AST.Expression

newtype Program = Program [FuncDef] deriving (Show, Generic)
type FuncDef = Common.FuncDef Name NamedSignature Stmt
type NamedSignature = Common.NamedSignature Name TypeExpr

data Stmt = BlockStmt [Stmt] 
          | WhileStmt Expr Stmt
          | ExprStmt Expr 
          | IfStmt Expr Stmt Stmt
          | AssignStmt Name Expr
          | VarStmt Name TypeExpr Expr
          | LetStmt Name TypeExpr Expr          
    deriving Show

data Expr = Identifier Name
          | FuncExpr NamedSignature Stmt
          | Call Expr [Expr]

          | LiteralExpr Literal
          | BinExpr (Binary Expr)
          | UnaryExpr (Unary Expr)
    deriving Show

instance Newtype Program