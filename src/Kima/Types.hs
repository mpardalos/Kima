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
    :: TypeCtx -> AST p Desugared -> Either TypecheckingError (AST p Typed)
typecheck typeCtx dAST = fst <$> typecheckWithTypeCtx typeCtx dAST

typecheckWithTypeCtx
    :: TypeCtx
    -> AST p Desugared
    -> Either TypecheckingError (AST p Typed, TypeCtx)
typecheckWithTypeCtx baseTypeCtx dAST = flip runStateT baseTypeCtx $ do
    typeAnnotatedAST <- resolveTypes dAST
    checkAnyAST typeAnnotatedAST
  where
    checkAnyAST :: MonadTC m => AST p TypeAnnotated -> m (AST p Typed)
    checkAnyAST ast@Program{} = checkProgram ast
    checkAnyAST ast@FuncDef{} = checkTopLevel ast
    checkAnyAST ast@DataDef{} = checkTopLevel ast
    checkAnyAST ast@LiteralE{} = fst <$> infer ast
    checkAnyAST ast@IdentifierE{} = fst <$> infer ast
    checkAnyAST ast@FuncExpr{} = fst <$> infer ast
    checkAnyAST ast@Call{} = fst <$> infer ast
    checkAnyAST ast@ExprStmt{} = fst <$> inferReturns ast
    checkAnyAST ast@Block{} = fst <$> inferReturns ast
    checkAnyAST ast@While{} = fst <$> inferReturns ast
    checkAnyAST ast@If{} = fst <$> inferReturns ast
    checkAnyAST ast@Assign{} = fst <$> inferReturns ast
    checkAnyAST ast@Var{} =  fst <$> inferReturns ast
    checkAnyAST ast@Let{} = fst <$> inferReturns ast
