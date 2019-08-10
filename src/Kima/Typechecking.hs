module Kima.Typechecking
    ( module E
    , typecheck
    , typecheckWithTypeCtx
    , TypecheckingError(..)
    )
where

import           Kima.Typechecking.TypeResolution
                                               as E
                                                ( resolveTypes )
import           Kima.Typechecking.TypeCtx      as E (TypeCtx(typeBindings))

import           Kima.Typechecking.Errors       as E (TypecheckingError(..))

import           Control.Monad.State
import           Kima.AST

typecheck
    :: TypeCtx
    -> AST p Desugared
    -> Either TypecheckingError (AST p Typed)
typecheck typeCtx dAST = fst <$> typecheckWithTypeCtx typeCtx dAST

typecheckWithTypeCtx
    :: forall p. TypeCtx
    -> AST p Desugared
    -> Either TypecheckingError (AST p Typed, TypeCtx)
typecheckWithTypeCtx baseTypeCtx dAST = do
    (typeAnnotatedAST, computedTypeBindings) <- runStateT (resolveTypes dAST) (typeBindings baseTypeCtx)
    runStateT (_ typeAnnotatedAST) (baseTypeCtx { typeBindings = computedTypeBindings })
