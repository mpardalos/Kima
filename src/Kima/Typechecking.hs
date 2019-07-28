module Kima.Typechecking
    ( module E
    , typecheck
    , typecheckWithTypeCtx
    , addTVars
    , TypecheckingError(..)
    )
where

import           Kima.Typechecking.TypeResolution
                                               as E
                                                ( resolveTypes )
import           Kima.Typechecking.ConstraintGen
                                               as E
                                                ( makeConstraints )
import           Kima.Typechecking.ConstraintSolving
                                               as E
                                                ( unify )
import           Kima.Typechecking.DomainCalculation
                                               as E
                                                ( makeDomains
                                                , makeDomainsWithTypeCtx
                                                )
import           Kima.Typechecking.TypeCtx      as E (TypeCtx(typeBindings))

import           Kima.Typechecking.Errors       as E (TypecheckingError(..))
import           Kima.Typechecking.Constraints  as E
                                                ( Domains
                                                , EqConstraintSet
                                                )

import qualified Data.Map                      as Map
import           Control.Monad.State
import           Kima.TypeVars
import           Kima.AST

-- | Add type variables to the names of an AST
addTVars
    :: AST p TypeAnnotated
    -> AST p TVars
addTVars = (`evalState` 0) . addIdAnnotations @(State Int)
    (state $ \n -> (TypeVar n, n+1))

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
    let tVarAST = addTVars typeAnnotatedAST
    let constraints = makeConstraints tVarAST
    (domains, finalTypeCtx) <- makeDomainsWithTypeCtx (baseTypeCtx { typeBindings = computedTypeBindings }) tVarAST
    substitution <- unify constraints domains
    resultAST <- traverseIdAnnotations (applySubstitution substitution) tVarAST
    return (resultAST, finalTypeCtx)
  where
    applySubstitution substitution tvar =
        case Map.lookup tvar substitution of
            Just t  -> Right t
            Nothing -> Left (NoSolution tvar)
