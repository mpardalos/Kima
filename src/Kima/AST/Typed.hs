module Kima.AST.Typed where

import Safe
import GHC.Generics
import Control.Newtype.Generics

import Kima.AST.Common hiding (NamedSignature, FuncDef)
import qualified Kima.AST.Common as Common
import Kima.AST.Expression
import Kima.KimaTypes

newtype Program = Program [FuncDef] deriving (Show, Generic)
type FuncDef = Common.FuncDef Name NamedSignature Stmt
type NamedSignature = Common.NamedSignature Name KType

data Stmt = BlockStmt [Stmt]
          | WhileStmt Expr Stmt
          | ExprStmt Expr
          | IfStmt Expr Stmt Stmt
          | AssignStmt Name Expr
          | VarStmt Name KType Expr
          | LetStmt Name KType Expr
    deriving Show

data Expr = Identifier Name KType
          | FuncExpr NamedSignature Stmt
          | Call Expr [Expr] KType
          | LiteralExpr Literal KType
          | BinExpr (Binary Expr) KType
          | UnaryExpr (Unary Expr) KType
    deriving Show

instance Newtype Program

exprType :: Expr -> KType
exprType (Identifier _ t) = t
exprType (FuncExpr sig _) = Common.returnType sig
exprType (Call _ _ t) = t
exprType (LiteralExpr _ t) = t
exprType (BinExpr _ t) = t
exprType (UnaryExpr _ t) = t

stmtRetType :: Stmt -> KType
stmtRetType (ExprStmt e) = exprType e
stmtRetType (IfStmt _ tBranch _) = stmtRetType tBranch
stmtRetType (BlockStmt blk) = case lastMay blk of
    Just stmt -> stmtRetType stmt
    Nothing -> KUnit
stmtRetType WhileStmt{}  = KUnit
stmtRetType AssignStmt{} = KUnit
stmtRetType VarStmt{}    = KUnit
stmtRetType LetStmt{}    = KUnit