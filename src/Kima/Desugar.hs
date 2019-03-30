module Kima.Desugar
    ( desugar
    )
where

import           Data.Bifunctor

import           Kima.AST

desugar :: ParsedAST p -> DesugaredAST p
-- The only cases that actually change
desugar (BinE    bin  ) = desugarBinary (desugar <$> bin)
desugar (UnaryE  unary) = desugarUnary (desugar <$> unary)

-- Basically just traverse
desugar (Program ast  ) = Program (desugar <$> ast)
desugar (DataDef name members) =
    DataDef name members
desugar (FuncDef name args rt body) =
    FuncDef name args rt (desugar body)
desugar (LiteralE     lit) = LiteralE lit
desugar (IdentifierE name) = IdentifierE (desugarIdentifier name)
desugar (FuncExpr args rt body) = FuncExpr args rt (desugar body)
desugar (Call callee args) = Call (desugar callee) (desugar <$> args)
desugar (ExprStmt expr   ) = ExprStmt (desugar expr)
desugar (Block    stmts  ) = Block (desugar <$> stmts)
desugar (Assign name expr) = Assign (desugarIdentifier name) (desugar expr)
desugar (Let name t expr ) = Let name t (desugar expr)
desugar (Var name t expr ) = Var name t (desugar expr)
desugar (While stmt      ) = While (bimap desugar desugar stmt)
desugar (If    stmt      ) = If (bimap desugar desugar stmt)

desugarIdentifier :: Identifier t -> Identifier t
desugarIdentifier (Identifier "print") = Builtin PrintFunc
desugarIdentifier (Identifier "input") = Builtin InputFunc
desugarIdentifier name = name
-- desugarName (Accessor n  ) = Accessor n

desugarBinary :: Binary (DesugaredAST 'Expr) -> DesugaredAST 'Expr
desugarBinary (Add     l r) = Call (IdentifierE $ Builtin AddOp) [l, r]
desugarBinary (Sub     l r) = Call (IdentifierE $ Builtin SubOp) [l, r]
desugarBinary (Div     l r) = Call (IdentifierE $ Builtin DivOp) [l, r]
desugarBinary (Mul     l r) = Call (IdentifierE $ Builtin MulOp) [l, r]
desugarBinary (Mod     l r) = Call (IdentifierE $ Builtin ModOp) [l, r]
desugarBinary (Less    l r) = Call (IdentifierE $ Builtin LTOp) [l, r]
desugarBinary (LessEq  l r) = Call (IdentifierE $ Builtin LTEOp) [l, r]
desugarBinary (Greater l r) = Call (IdentifierE $ Builtin GTOp) [l, r]
desugarBinary (GreatEq l r) = Call (IdentifierE $ Builtin GTEOp) [l, r]
desugarBinary (Eq      l r) = Call (IdentifierE $ Builtin EqualsOp) [l, r]
desugarBinary (NotEq   l r) = Call
    (IdentifierE $ Builtin NegateOp)
    [Call (IdentifierE $ Builtin EqualsOp) [l, r]]

desugarUnary :: Unary (DesugaredAST 'Expr) -> DesugaredAST 'Expr
desugarUnary (Negate e) = Call (IdentifierE $ Builtin NegateOp) [e]
desugarUnary (Invert e) = Call (IdentifierE $ Builtin InvertOp) [e]
