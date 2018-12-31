module Kima.Typechecking
    ( module E
    , typecheck
    , TypecheckingError(..)
    )
where

import           Kima.Typechecking.ConstraintGen
                                               as E
                                                ( makeConstraints
                                                , ConstraintGenerationError
                                                )
import           Kima.Typechecking.ConstraintSolving
                                               as E
                                                ( UnificationError
                                                , unify
                                                )
import           Kima.Typechecking.Types       as E
                                                ( SomeConstraint
                                                , SomeConstraintSet
                                                , TVarAST
                                                , TVarProgram
                                                )

import           Kima.Typechecking.Types
import qualified Data.Map                      as Map
import           Data.Bifunctor
import           Kima.AST

data TypecheckingError = UnificationError UnificationError
                       | ConstraintGenerationError ConstraintGenerationError
                       | NoSolution TypeVar
    deriving Show

typecheck
    :: DesugaredProgram -> Either TypecheckingError TypedProgram
typecheck dAST = do
    (tVarAST, constraints) <- first ConstraintGenerationError
                                    (makeConstraints dAST)
    substitution <- first UnificationError (unify constraints)
    traverseNames (applySubstitution substitution) tVarAST
  where
    applySubstitution substitution name =
        case Map.lookup (nameType name) substitution of
            Just t  -> pure (typeAnnotate t name)
            Nothing -> Left (NoSolution (nameType name))
