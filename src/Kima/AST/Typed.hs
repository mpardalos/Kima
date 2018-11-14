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
type NamedSignature = Common.NamedSignature Name (KType 'Overload)

data Stmt = BlockStmt [Stmt]
          | WhileStmt Expr Stmt
          | ExprStmt Expr
          | IfStmt Expr Stmt Stmt
          | AssignStmt Name Expr
          | VarStmt Name (KType 'Overload) Expr
          | LetStmt Name (KType 'Overload) Expr
    deriving Show

data Expr = Identifier Name (KType 'Overload)
          | FuncExpr NamedSignature Stmt
          | Call Expr [Expr] (KType 'Overload)
          | LiteralExpr Literal (KType 'Overload)
          | BinExpr (Binary Expr) (KType 'Overload)
          | UnaryExpr (Unary Expr) (KType 'Overload)
    deriving Show

instance Newtype Program

exprType :: Expr -> KType 'Overload
exprType (Identifier _ t) = t
exprType (FuncExpr sig _) = Common.returnType sig
exprType (Call _ _ t) = t
exprType (LiteralExpr _ t) = t
exprType (BinExpr _ t) = t
exprType (UnaryExpr _ t) = t

stmtRetType :: Stmt -> KType 'Overload
stmtRetType (ExprStmt e) = exprType e
stmtRetType (IfStmt _ tBranch _) = stmtRetType tBranch
stmtRetType (BlockStmt blk) = case lastMay blk of
    Just stmt -> stmtRetType stmt
    Nothing -> KUnit
stmtRetType WhileStmt{}  = KUnit
stmtRetType AssignStmt{} = KUnit
stmtRetType VarStmt{}    = KUnit
stmtRetType LetStmt{}    = KUnit