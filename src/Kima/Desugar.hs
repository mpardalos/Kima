module Kima.Desugar where

import Control.Newtype.Generics
import Data.Bifunctor

import Kima.AST.Expression
import Kima.AST.Desugared as D
import Kima.AST.Typed as T

desugarProgram :: T.Program -> D.Program
desugarProgram = over T.Program (fmap desugarFuncDef)

desugarFuncDef :: T.FuncDef -> D.FuncDef
desugarFuncDef = bimap (const ()) desugarStmt

desugarExpr :: T.Expr -> D.Expr
desugarExpr (T.Identifier name _)  = D.Identifier name
desugarExpr (T.FuncExpr sig body)  = D.FuncExpr (() <$ sig) (desugarStmt body)
desugarExpr (T.Call callee args _) = D.Call (desugarExpr callee) (desugarExpr <$> args)
desugarExpr (T.LiteralExpr lit _)  = D.LiteralExpr lit
desugarExpr (T.BinExpr bin _)      = desugarBinary (desugarExpr <$> bin)
desugarExpr (T.UnaryExpr unary _)  = desugarUnary (desugarExpr <$> unary)

desugarStmt :: T.Stmt -> D.Stmt
desugarStmt (T.BlockStmt body)            = D.BlockStmt (desugarStmt <$> body)
desugarStmt (T.WhileStmt cond body)       = D.WhileStmt (desugarExpr cond) (desugarStmt body)
desugarStmt (T.ExprStmt expr)             = D.ExprStmt (desugarExpr expr)
desugarStmt (T.IfStmt cond ifBlk elseBlk) = D.IfStmt (desugarExpr cond) (desugarStmt ifBlk) (desugarStmt elseBlk)
desugarStmt (T.AssignStmt name expr)      = D.AssignStmt name (desugarExpr expr)
desugarStmt (T.VarStmt name _ expr)       = D.AssignStmt name (desugarExpr expr)
desugarStmt (T.LetStmt name _ expr)       = D.AssignStmt name (desugarExpr expr)

desugarBinary :: Binary D.Expr -> D.Expr
desugarBinary (Add l r) = D.Call (D.Identifier "b__add") [l, r]
desugarBinary (Sub l r) = D.Call (D.Identifier "b__sub") [l, r]
desugarBinary (Div l r) = D.Call (D.Identifier "b__div") [l, r]
desugarBinary (Mul l r) = D.Call (D.Identifier "b__mul") [l, r]
desugarBinary (Mod l r) = D.Call (D.Identifier "b__mod") [l, r]

desugarUnary :: Unary D.Expr -> D.Expr
desugarUnary (Negate e) = D.Call (D.Identifier "b__negate") [e]
desugarUnary (Invert e) = D.Call (D.Identifier "b__invert") [e]