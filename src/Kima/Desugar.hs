module Kima.Desugar (desugar) where

import Data.Bifunctor

import Kima.AST

desugar :: ParsedAST p -> DesugaredAST p
-- The only cases that actually change
desugar (BinE bin) = desugarBinary (desugar <$> bin)
desugar (UnaryE unary) = desugarUnary (desugar <$> unary)

-- Basically just traverse
desugar (Program ast) = Program (desugar <$> ast) 
desugar (FuncDefAnn name args rt body) = FuncDefAnn (desugarName name) (first desugarName <$> args) rt (desugar body)
desugar (LiteralE lit) = LiteralE lit
desugar (Identifier name) = Identifier (desugarName name)
desugar (FuncExprAnn args rt body) = FuncExprAnn (first desugarName <$> args) rt (desugar body)
desugar (Call callee args) = Call (desugar callee) (desugar <$> args)
desugar (ExprStmt expr) = ExprStmt (desugar expr) 
desugar (Block stmts) = Block (desugar <$> stmts)
desugar (Assign name expr) = Assign (desugarName name) (desugar expr) 
desugar (Let name t expr) = Let (desugarName name) t (desugar expr)
desugar (Var name t expr) = Let (desugarName name) t (desugar expr)
desugar (While stmt) = While (bimap desugar desugar stmt)
desugar (If stmt) = If (bimap desugar desugar stmt)

desugarName :: ParsedName -> DesugaredName
desugarName (Name n) = Name n

desugarBinary :: Binary (DesugaredAST 'Expr) -> DesugaredAST 'Expr
desugarBinary (Add l r) = Call (Identifier $ Builtin AddOp) [l, r]
desugarBinary (Sub l r) = Call (Identifier $ Builtin SubOp) [l, r]
desugarBinary (Div l r) = Call (Identifier $ Builtin DivOp) [l, r]
desugarBinary (Mul l r) = Call (Identifier $ Builtin MulOp) [l, r]
desugarBinary (Mod l r) = Call (Identifier $ Builtin ModOp) [l, r]

desugarUnary :: Unary (DesugaredAST 'Expr)-> DesugaredAST 'Expr
desugarUnary (Negate e) = Call (Identifier $ Builtin NegateOp) [e]
desugarUnary (Invert e) = Call (Identifier $ Builtin InvertOp) [e]