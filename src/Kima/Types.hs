module Kima.Types
    ( module E
    , typecheck
    , typecheckWithTypeCtx
    , TypecheckingError(..)
    )
where

import           Kima.Types.TypeResolution
                                               as E
                                                ( resolveTypes )
import           Kima.Types.TypeCtx     as E
                                                ( TypeCtx(typeBindings) )

import           Kima.Types.Errors      as E
                                                ( TypecheckingError(..) )
import           Kima.Types.Bidirectional
                                               as E
                                                ( MonadTC
                                                , checkProgram
                                                , checkTopLevel
                                                , check
                                                , infer
                                                , checkReturns
                                                , inferReturns
                                                )

import           Control.Monad.State
import           Kima.AST

typecheck
    :: TypeCtx -> AST Desugared -> Either TypecheckingError (AST Typed)
typecheck typeCtx dAST = fst <$> typecheckWithTypeCtx typeCtx dAST

typecheckWithTypeCtx
    :: TypeCtx -> AST Desugared -> Either TypecheckingError (AST Typed, TypeCtx)
typecheckWithTypeCtx baseTypeCtx dAST = flip runStateT baseTypeCtx $ do
    typeAnnotatedAST <- resolveTypes dAST
    checkAnyAST typeAnnotatedAST
  where
    checkAnyAST :: MonadTC m => AST TypeAnnotated -> m (AST Typed)
    checkAnyAST (ModuleAST   ast) = ModuleAST <$> checkProgram ast
    checkAnyAST (TopLevelAST ast) = TopLevelAST <$> checkTopLevel ast
    checkAnyAST (ExprAST     ast) = ExprAST . fst <$> infer ast
    checkAnyAST (StmtAST     ast) = StmtAST . fst <$> inferReturns ast
