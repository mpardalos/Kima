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
import           Kima.Typechecking.Types       as E
                                                ( AnnotatedTVarAST
                                                , AnnotatedTVarProgram
                                                , Domains
                                                , EqConstraintSet
                                                , TVarAST
                                                , TVarProgram
                                                , TypecheckingError(..)
                                                , TypeVar(..)
                                                , TypeCtx
                                                )

import qualified Data.Map                      as Map
import           Control.Monad.State
import           Kima.AST

-- | Add type variables to the names of an AST
addTVars = (`evalState` 0) . traverseNames @(State Int)
    (\name -> state $ \n -> (typeAnnotate (TypeVar n) name, n + 1))

typecheck :: TypeCtx -> DesugaredAST p -> Either TypecheckingError (TypedAST p)
typecheck typeCtx dAST = fst <$> (typecheckWithTypeCtx typeCtx dAST)

typecheckWithTypeCtx :: TypeCtx -> DesugaredAST p -> Either TypecheckingError (TypedAST p, TypeCtx)
typecheckWithTypeCtx typeCtx dAST = do
    typeAnnotatedAST <- resolveTypes dAST
    let tVarAST = addTVars typeAnnotatedAST
    let constraints = makeConstraints tVarAST
    (domains, finalTypeCtx) <- makeDomainsWithTypeCtx typeCtx tVarAST
    substitution <- unify constraints domains
    resultAST <- removeTypeAnnotations
        <$> traverseNames (applySubstitution substitution) tVarAST
    return (resultAST, finalTypeCtx)
  where
    applySubstitution substitution name =
        case Map.lookup (nameType name) substitution of
            Just t  -> pure (typeAnnotate t name)
            Nothing -> Left (NoSolution (nameType name))