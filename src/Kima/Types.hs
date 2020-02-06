module Kima.Types
    ( module E
    , typecheckAST
    , typecheckModule
    , typecheckTopLevel
    , typecheckStmt
    , typecheckExpr
    , typecheckASTWithTypeCtx
    , typecheckModuleWithTypeCtx
    , typecheckTopLevelWithTypeCtx
    , typecheckStmtWithTypeCtx
    , typecheckExprWithTypeCtx
    , TypecheckingError(..)
    )
where

import           Kima.Types.TypeResolution     as E
                                                ( resolveASTTypes
                                                , resolveModuleTypes
                                                , resolveTopLevelTypes
                                                , resolveStmtTypes
                                                , resolveExprTypes
                                                )
import           Kima.Types.TypeCtx            as E
                                                ( TypeCtx(typeBindings) )

import           Kima.Types.Errors             as E
                                                ( TypecheckingError(..) )
import           Kima.Types.Bidirectional      as E
                                                ( MonadTC
                                                , checkProgram
                                                , checkTopLevel
                                                , check
                                                , infer
                                                , checkReturns
                                                , inferReturns
                                                )

import           Kima.AST
import           Control.Monad.State
import           Data.Functor                   ( (<&>) )
import           Data.Bifunctor                 ( first )

typecheckAST :: TypeCtx -> AST Desugared -> Either TypecheckingError (AST Typed)
typecheckAST typeCtx = fmap fst . typecheckASTWithTypeCtx typeCtx

typecheckModule
    :: TypeCtx -> Module Desugared -> Either TypecheckingError (Module Typed)
typecheckModule typeCtx = fmap fst . typecheckModuleWithTypeCtx typeCtx

typecheckTopLevel
    :: TypeCtx
    -> TopLevel Desugared
    -> Either TypecheckingError (TopLevel Typed)
typecheckTopLevel typeCtx = fmap fst . typecheckTopLevelWithTypeCtx typeCtx

typecheckStmt
    :: TypeCtx -> Stmt Desugared -> Either TypecheckingError (Stmt Typed)
typecheckStmt typeCtx = fmap fst . typecheckStmtWithTypeCtx typeCtx

typecheckExpr
    :: TypeCtx -> Expr Desugared -> Either TypecheckingError (Expr Typed)
typecheckExpr typeCtx = fmap fst . typecheckExprWithTypeCtx typeCtx

typecheckASTWithTypeCtx
    :: TypeCtx -> AST Desugared -> Either TypecheckingError (AST Typed, TypeCtx)
typecheckASTWithTypeCtx baseTypeCtx (ModuleAST ast) =
    first ModuleAST <$> typecheckModuleWithTypeCtx baseTypeCtx ast
typecheckASTWithTypeCtx baseTypeCtx (TopLevelAST ast) =
    first TopLevelAST <$> typecheckTopLevelWithTypeCtx baseTypeCtx ast
typecheckASTWithTypeCtx baseTypeCtx (StmtAST ast) =
    first StmtAST <$> typecheckStmtWithTypeCtx baseTypeCtx ast
typecheckASTWithTypeCtx baseTypeCtx (ExprAST ast) =
    first ExprAST <$> typecheckExprWithTypeCtx baseTypeCtx ast

typecheckModuleWithTypeCtx
    :: TypeCtx
    -> Module Desugared
    -> Either TypecheckingError (Module Typed, TypeCtx)
typecheckModuleWithTypeCtx baseTypeCtx dAST =
    (resolveModuleTypes dAST >>= checkProgram) `runStateT` baseTypeCtx

typecheckTopLevelWithTypeCtx
    :: TypeCtx
    -> TopLevel Desugared
    -> Either TypecheckingError (TopLevel Typed, TypeCtx)
typecheckTopLevelWithTypeCtx baseTypeCtx dAST =
    (resolveTopLevelTypes dAST >>= checkTopLevel) `runStateT` baseTypeCtx

typecheckStmtWithTypeCtx
    :: TypeCtx
    -> Stmt Desugared
    -> Either TypecheckingError (Stmt Typed, TypeCtx)
typecheckStmtWithTypeCtx baseTypeCtx dAST =
    ((resolveStmtTypes dAST >>= inferReturns) <&> fst) `runStateT` baseTypeCtx

typecheckExprWithTypeCtx
    :: TypeCtx
    -> Expr Desugared
    -> Either TypecheckingError (Expr Typed, TypeCtx)
typecheckExprWithTypeCtx baseTypeCtx dAST =
    ((resolveExprTypes dAST >>= infer) <&> fst) `runStateT` baseTypeCtx
