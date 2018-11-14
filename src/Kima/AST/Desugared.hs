module Kima.AST.Desugared where

import GHC.Generics
import Control.Newtype.Generics

import Kima.AST.Expression
import Kima.AST.Common hiding (NamedSignature, FuncDef)
import qualified Kima.AST.Common as Common
import Kima.KimaTypes

data BuiltinName = AddOp | SubOp | DivOp | MulOp | ModOp 
                 | NegateOp | InvertOp
                 | PrintFunc
    deriving (Show, Eq, Ord)

data RuntimeName = TypedName Name (KType 'NoOverload)
                 | Builtin BuiltinName
    deriving (Show, Eq, Ord)

newtype Program = Program [Kima.AST.Desugared.FuncDef] deriving (Show, Generic)
type FuncDef = Common.FuncDef RuntimeName NamedSignature Stmt
type NamedSignature = [RuntimeName]

data Expr = LiteralExpr Literal 
          | Identifier RuntimeName
          | FuncExpr NamedSignature Stmt
          | Call Expr [Expr]
    deriving Show

data Stmt = BlockStmt [Stmt]
          | AssignStmt RuntimeName Expr
          | WhileStmt Expr Stmt
          | ExprStmt Expr 
          | IfStmt Expr Stmt Stmt
    deriving Show

instance Newtype Program
instance Show FuncDef where
    show Common.FuncDef { name, body } =
        "fun " ++ show name 
        ++ " (?) -> ? "
        ++ show body