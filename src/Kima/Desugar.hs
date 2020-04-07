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
desugarTopLevel (OperationDef name args rt) = OperationDef
    name
    (second (fmap desugarTypeExpr) <$> args)
    (desugarTypeExpr <$> rt)
desugarTopLevel (EffectSynonymDef name ops) = EffectSynonymDef name ops

desugarStmt :: Stmt Parsed -> Stmt Desugared
desugarStmt (SimpleIf cond body) =
    If (IfStmt (desugarExpr cond) (desugarStmt body) (Block []))

desugarStmt (ExprStmt expr     ) = ExprStmt (desugarExpr expr)
desugarStmt (Block    stmts    ) = Block (desugarStmt <$> stmts)
desugarStmt (Assign target expr) = Assign target (desugarExpr expr)
desugarStmt (Let name t expr   ) = Let name (desugarTypeExpr <$> t) (desugarExpr expr)
desugarStmt (Var name t expr   ) = Var name (desugarTypeExpr <$> t) (desugarExpr expr)
desugarStmt (While stmt        ) = While (bimap desugarExpr desugarStmt stmt)
desugarStmt (If    stmt        ) = If (bimap desugarExpr desugarStmt stmt)

desugarExpr :: Expr Parsed -> Expr Desugared
desugarExpr (BinE    op l r  )  = CallExpr (IdentifierExpr $ Builtin (BinaryOp op)) [desugarExpr l, desugarExpr r]
desugarExpr (UnaryE  op e    )  = CallExpr (IdentifierExpr $ Builtin (UnaryOp op)) [desugarExpr e]
desugarExpr (AccessExpr expr field) = CallExpr (IdentifierExpr (Accessor field)) [desugarExpr expr]
desugarExpr (LiteralExpr    lit                 ) = LiteralExpr lit
desugarExpr (IdentifierExpr name) = IdentifierExpr name
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
desugarExpr (CallExpr callee args  ) = CallExpr (desugarExpr callee) (desugarExpr <$> args)
desugarExpr (HandleExpr callee handlers) = HandleExpr (desugarExpr callee) (desugarHandler <$> handlers)

desugarHandler :: HandlerClause Parsed -> HandlerClause Desugared
desugarHandler (HandlerClause name args rt body) = HandlerClause
    name
    (fmap (fmap desugarTypeExpr) <$> args)
    (desugarTypeExpr <$> rt)
    (desugarStmt body)

desugarTypeExpr :: ParsedTypeExpr -> TypeExpr
desugarTypeExpr (ParsedTypeName name) = TypeName name
desugarTypeExpr (ParsedSignatureType args (Just eff) rt) =
    SignatureType (desugarTypeExpr <$> args) eff (desugarTypeExpr rt)
desugarTypeExpr (ParsedSignatureType args Nothing rt) =
    SignatureType (desugarTypeExpr <$> args) [] (desugarTypeExpr rt)
