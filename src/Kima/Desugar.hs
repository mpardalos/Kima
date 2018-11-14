module Kima.Desugar where

import Control.Newtype.Generics

import Kima.AST.Common
import Kima.AST.Desugared as D
import Kima.AST.Expression
import Kima.AST.Typed as T
import Kima.KimaTypes(KType(..), ($->))

desugarProgram :: T.Program -> D.Program
desugarProgram = over T.Program (fmap desugarFuncDef)

desugarFuncDef :: T.FuncDef -> D.FuncDef
desugarFuncDef FuncDef{ name, signature, body } = FuncDef 
    (TypedName name (KFunc (argTypes $-> returnType signature))) 
    (desugarNamedSignature signature)
    (desugarStmt body)
    where
        -- argTypes :: [KType]
        argTypes = snd <$> arguments signature


desugarNamedSignature :: T.NamedSignature -> D.NamedSignature
desugarNamedSignature NamedSignature { arguments } = uncurry TypedName <$> arguments

desugarExpr :: T.Expr -> D.Expr
desugarExpr (T.Identifier "print" _) = D.Identifier (Builtin PrintFunc)
desugarExpr (T.Identifier name t)    = D.Identifier (TypedName name _t)
desugarExpr (T.FuncExpr sig body)    = D.FuncExpr (desugarNamedSignature sig) (desugarStmt body)
desugarExpr (T.Call callee args _)   = D.Call (desugarExpr callee) (desugarExpr <$> args)
desugarExpr (T.LiteralExpr lit _)    = D.LiteralExpr lit
desugarExpr (T.BinExpr bin _)        = desugarBinary (desugarExpr <$> bin)
desugarExpr (T.UnaryExpr unary _)    = desugarUnary (desugarExpr <$> unary)

desugarStmt :: T.Stmt -> D.Stmt
desugarStmt (T.BlockStmt body)            = D.BlockStmt (desugarStmt <$> body)
desugarStmt (T.WhileStmt cond body)       = D.WhileStmt (desugarExpr cond) (desugarStmt body)
desugarStmt (T.ExprStmt expr)             = D.ExprStmt (desugarExpr expr)
desugarStmt (T.IfStmt cond ifBlk elseBlk) = D.IfStmt (desugarExpr cond) (desugarStmt ifBlk) (desugarStmt elseBlk)
desugarStmt (T.AssignStmt name expr)      = D.AssignStmt (TypedName name (exprType expr)) (desugarExpr expr)
desugarStmt (T.VarStmt name _ expr)       = D.AssignStmt (TypedName name (exprType expr)) (desugarExpr expr)
desugarStmt (T.LetStmt name _ expr)       = D.AssignStmt (TypedName name (exprType expr)) (desugarExpr expr)

desugarBinary :: Binary D.Expr -> D.Expr
desugarBinary (Add l r) = D.Call (D.Identifier (Builtin AddOp)) [l, r]
desugarBinary (Sub l r) = D.Call (D.Identifier (Builtin SubOp)) [l, r]
desugarBinary (Div l r) = D.Call (D.Identifier (Builtin DivOp)) [l, r]
desugarBinary (Mul l r) = D.Call (D.Identifier (Builtin MulOp)) [l, r]
desugarBinary (Mod l r) = D.Call (D.Identifier (Builtin ModOp)) [l, r]

desugarUnary :: Unary D.Expr -> D.Expr
desugarUnary (Negate e) = D.Call (D.Identifier (Builtin NegateOp)) [e]
desugarUnary (Invert e) = D.Call (D.Identifier (Builtin InvertOp)) [e]