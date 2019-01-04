module Kima.Typechecking
    ( module E
    , typecheck
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
                                                ( makeDomains )
import           Kima.Typechecking.Types       as E
                                                ( AnnotatedTVarAST
                                                , AnnotatedTVarProgram
                                                , Domains
                                                , EqConstraintSet
                                                , TVarAST
                                                , TVarProgram
                                                , TypecheckingError(..)
                                                , TypeVar
                                                )
import           Kima.Typechecking.Types        ( TypeVar(..) )

import qualified Data.Map                      as Map
import           Control.Monad.State
import           Kima.AST

-- | Add type variables to the names of an AST
addTVars = (`evalState` 0) . traverseNames @(State Int)
    (\name -> state $ \n -> (typeAnnotate (TypeVar n) name, n + 1))

typecheck :: DesugaredProgram -> Either TypecheckingError TypedProgram
typecheck dAST = do
    typeAnnotatedAST <- resolveTypes dAST
    let tVarAST = addTVars typeAnnotatedAST
    let constraints = makeConstraints tVarAST
    domains      <- makeDomains tVarAST
    substitution <- unify constraints domains
    removeTypeAnnotations
        <$> traverseNames (applySubstitution substitution) tVarAST
  where
    applySubstitution substitution name =
        case Map.lookup (nameType name) substitution of
            Just t  -> pure (typeAnnotate t name)
            Nothing -> Left (NoSolution (nameType name))

