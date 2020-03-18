{-# LANGUAGE OverloadedLists #-}
module Kima.Desugar
    ( desugarModule
    , desugarTopLevel
    , desugarStmt
    , desugarExpr
    )
where

import           Data.Bifunctor

import           Kima.AST

desugarModule :: Module Parsed -> Module Desugared
desugarModule (Program ast) = Program (desugarTopLevel <$> ast)

desugarTopLevel :: TopLevel Parsed -> TopLevel Desugared
desugarTopLevel (DataDef name members) =
    DataDef name (second (fmap desugarTypeExpr) <$> members)
desugarTopLevel (FuncDef name args (Just eff) rt body) = FuncDef
    name
    (second (fmap desugarTypeExpr) <$> args)
    eff
    (desugarTypeExpr <$> rt)
    (desugarStmt body)
desugarTopLevel (FuncDef name args Nothing rt body) = FuncDef
    name
    (second (fmap desugarTypeExpr) <$> args)
    []
    (desugarTypeExpr <$> rt)
    (desugarStmt body)

desugarStmt :: Stmt Parsed -> Stmt Desugared
desugarStmt (ExprStmt expr     ) = ExprStmt (desugarExpr expr)
desugarStmt (Block    stmts    ) = Block (desugarStmt <$> stmts)
desugarStmt (Assign target expr) = Assign target (desugarExpr expr)
desugarStmt (Let name t expr   ) = Let name (desugarTypeExpr <$> t) (desugarExpr expr)
desugarStmt (Var name t expr   ) = Var name (desugarTypeExpr <$> t) (desugarExpr expr)
desugarStmt (While stmt        ) = While (bimap desugarExpr desugarStmt stmt)
desugarStmt (If    stmt        ) = If (bimap desugarExpr desugarStmt stmt)

desugarExpr :: Expr Parsed -> Expr Desugared
desugarExpr (BinE    bin  )  = desugarBinary (desugarExpr <$> bin)
desugarExpr (UnaryE  unary)  = desugarUnary (desugarExpr <$> unary)
desugarExpr (AccessE expr field) = Call (IdentifierE (Accessor field)) [desugarExpr expr]
desugarExpr (LiteralE    lit                 ) = LiteralE lit
desugarExpr (IdentifierE name) = IdentifierE (desugarIdentifier name)
desugarExpr (FuncExpr args (Just eff) rt body) = FuncExpr
    (second (fmap desugarTypeExpr) <$> args)
    eff
    (desugarTypeExpr <$> rt)
    (desugarStmt body)
desugarExpr (FuncExpr args Nothing rt body) = FuncExpr
    (second (fmap desugarTypeExpr) <$> args)
    []
    (desugarTypeExpr <$> rt)
    (desugarStmt body)
desugarExpr (Call callee args  ) = Call (desugarExpr callee) (desugarExpr <$> args)

desugarIdentifier :: Identifier t -> Identifier t
desugarIdentifier (Identifier "print") = Builtin PrintFunc
desugarIdentifier (Identifier "input") = Builtin InputFunc
desugarIdentifier name = name

desugarBinary
    :: ( TagSugar tag ~ 'NoSugar
       , NameAnnotation tag ~ 'NoAnnotation)
    => Binary (Expr tag) -> Expr tag
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
    => Unary (Expr tag) -> Expr tag
desugarUnary (Negate e) = Call (IdentifierE $ Builtin NegateOp) [e]
desugarUnary (Invert e) = Call (IdentifierE $ Builtin InvertOp) [e]

desugarTypeExpr :: ParsedTypeExpr -> TypeExpr
desugarTypeExpr (ParsedTypeName name) = TypeName name
desugarTypeExpr (ParsedSignatureType args (Just eff) rt) =
    SignatureType (desugarTypeExpr <$> args) eff (desugarTypeExpr rt)
desugarTypeExpr (ParsedSignatureType args Nothing rt) =
    SignatureType (desugarTypeExpr <$> args) [] (desugarTypeExpr rt)
