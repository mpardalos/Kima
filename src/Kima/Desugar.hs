module Kima.Desugar
    ( desugar
    )
where

import           Data.Bifunctor

import           Kima.AST

desugar :: AST p Parsed -> AST p Desugared
desugar (BinE    bin  )  = desugarBinary (desugar <$> bin)
desugar (UnaryE  unary)  = desugarUnary (desugar <$> unary)
desugar (AccessE expr field) = Call (IdentifierE (Accessor field)) [desugar expr]
desugar (Program ast) = Program (desugar <$> ast)
desugar (DataDef name members) =
    DataDef name (second (fmap desugarTypeExpr) <$> members)
desugar (FuncDef name args (Just eff) rt body) = FuncDef
    name
    (second (fmap desugarTypeExpr) <$> args)
    eff
    (desugarTypeExpr <$> rt)
    (desugar body)
desugar (FuncDef name args Nothing rt body) = FuncDef
    name
    (second (fmap desugarTypeExpr) <$> args)
    noEffect
    (desugarTypeExpr <$> rt)
    (desugar body)
desugar (LiteralE    lit                 ) = LiteralE lit
desugar (IdentifierE name) = IdentifierE (desugarIdentifier name)
desugar (FuncExpr args (Just eff) rt body) = FuncExpr
    (second (fmap desugarTypeExpr) <$> args)
    eff
    (desugarTypeExpr <$> rt)
    (desugar body)
desugar (FuncExpr args Nothing rt body) = FuncExpr
    (second (fmap desugarTypeExpr) <$> args)
    noEffect
    (desugarTypeExpr <$> rt)
    (desugar body)
desugar (Call callee args  ) = Call (desugar callee) (desugar <$> args)
desugar (ExprStmt expr     ) = ExprStmt (desugar expr)
desugar (Block    stmts    ) = Block (desugar <$> stmts)
desugar (Assign target expr) = Assign target (desugar expr)
desugar (Let name t expr   ) = Let name (desugarTypeExpr <$> t) (desugar expr)
desugar (Var name t expr   ) = Var name (desugarTypeExpr <$> t) (desugar expr)
desugar (While stmt        ) = While (bimap desugar desugar stmt)
desugar (If    stmt        ) = If (bimap desugar desugar stmt)

desugarIdentifier :: Identifier t -> Identifier t
desugarIdentifier (Identifier "print") = Builtin PrintFunc
desugarIdentifier (Identifier "input") = Builtin InputFunc
desugarIdentifier name = name

desugarBinary
    :: ( TagSugar tag ~ 'NoSugar
       , NameAnnotation tag ~ 'NoAnnotation)
    => Binary (AST 'Expr tag) -> AST 'Expr tag
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

desugarUnary
    :: ( TagSugar tag ~ 'NoSugar
       , NameAnnotation tag ~ 'NoAnnotation)
    => Unary (AST 'Expr tag) -> AST 'Expr tag
desugarUnary (Negate e) = Call (IdentifierE $ Builtin NegateOp) [e]
desugarUnary (Invert e) = Call (IdentifierE $ Builtin InvertOp) [e]

desugarTypeExpr :: ParsedTypeExpr -> TypeExpr
desugarTypeExpr (ParsedTypeName name) = TypeName name
desugarTypeExpr (ParsedSignatureType args (Just eff) rt) =
    SignatureType (desugarTypeExpr <$> args) eff (desugarTypeExpr rt)
desugarTypeExpr (ParsedSignatureType args Nothing rt) =
    SignatureType (desugarTypeExpr <$> args) noEffect (desugarTypeExpr rt)
