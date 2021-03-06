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
desugarModule (Module ast) = Module (desugarTopLevel <$> ast)

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
desugarStmt (SimpleIfStmt cond body) =
    IfStmt (If (desugarExpr cond) (desugarStmt body) (BlockStmt []))
desugarStmt SimpleBreakStmt          = BreakStmt (LiteralExpr UnitLit)

desugarStmt (ExprStmt expr     ) = ExprStmt (desugarExpr expr)
desugarStmt (BlockStmt    stmts    ) = BlockStmt (desugarStmt <$> stmts)
desugarStmt (AssignStmt target expr) = AssignStmt target (desugarExpr expr)
desugarStmt (LetStmt name t expr   ) = LetStmt name (desugarTypeExpr <$> t) (desugarExpr expr)
desugarStmt (VarStmt name t expr   ) = VarStmt name (desugarTypeExpr <$> t) (desugarExpr expr)
desugarStmt (WhileStmt stmt        ) = WhileStmt (bimap desugarExpr desugarStmt stmt)
desugarStmt (IfStmt    stmt        ) = IfStmt (bimap desugarExpr desugarStmt stmt)
desugarStmt (BreakStmt expr        ) = BreakStmt (desugarExpr expr)

desugarExpr :: Expr Parsed -> Expr Desugared
desugarExpr (BinExpr    op l r  )  = CallExpr (IdentifierExpr $ Builtin (BinaryOp op)) [desugarExpr l, desugarExpr r]
desugarExpr (UnaryExpr  op e    )  = CallExpr (IdentifierExpr $ Builtin (UnaryOp op)) [desugarExpr e]
desugarExpr (AccessExpr expr field) = CallExpr (IdentifierExpr (Accessor field)) [desugarExpr expr]
desugarExpr (SimpleHandleExpr expr handlers) = HandleExpr (ExprStmt (desugarExpr expr)) (desugarHandler <$> handlers)

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
desugarExpr (HandleExpr stmt handlers) = HandleExpr (desugarStmt stmt) (desugarHandler <$> handlers)

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
