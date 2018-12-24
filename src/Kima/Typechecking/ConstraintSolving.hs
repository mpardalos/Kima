module Kima.Typechecking.ConstraintSolving where

import           Control.Monad.Except
import           Control.Arrow
import           Data.Map                       ( Map
                                                , (!?)
                                                )
import qualified Data.Map                      as Map
import           Data.Set                      as Set

import           Kima.KimaTypes
import           Kima.Typechecking.Types


unify :: ConstraintSet -> UnifyResult Substitution
unify = reduceConstraints >=> uncurry unifyEqualities

data UnificationError = DomainMismatch
                      | UnboundVariable TypeVar
                      | CantUnify TypeVar TypeVar
                      | FailureConstraint
    deriving Show

type Domains = Map TypeVar (Set KType)
type UnifyResult a = Either UnificationError a

reduceConstraints
    :: ReducedConstraintSet a -> UnifyResult (Domains, EqConstraintSet)
reduceConstraints (AsEquality c : cs) = second (c :) <$> reduceConstraints cs
reduceConstraints (IsOneOf tvar domain : cs) = do
    (allDomains, reducedConstraints) <- reduceConstraints cs
    case Map.lookup tvar allDomains of
        Just oldDomain ->
            unless (domain `isSubset` oldDomain) (throwError DomainMismatch)
        Nothing -> pure ()
    return (Map.insert tvar domain allDomains, reducedConstraints)
reduceConstraints (Failure : _) = throwError FailureConstraint
reduceConstraints [] = Right (Map.empty, [])
reduceConstraints c = error ("Could not pattern match constraint: " <> show c)

unifyEqualities :: Domains -> EqConstraintSet -> UnifyResult Substitution
unifyEqualities _       []                = pure Map.empty
unifyEqualities domains (eq@Equal{} : cs) = do
    (newDomains, substitution) <- unifyEquality domains eq
    restSubstitutions          <- unifyEqualities newDomains cs
    return (substitution <> restSubstitutions)

-- | Try to unify two typevars under a given set of domains.
-- | Return the updated domains and a the substitution that unifies the two 
-- | typevars. 
unifyEquality :: Domains -> EqConstraint -> UnifyResult (Domains, Substitution)
-- Unify concrete types
unifyEquality ds (Equal (TheType t1) (TheType t2)) | t1 == t2 =
    pure (ds, Map.empty)
unifyEquality _ (Equal (TheType t1) (TheType t2)) | t1 /= t2 =
    throwError (CantUnify (TheType t1) (TheType t2))
-- Unify vars to concrete types
unifyEquality ds (Equal var@TypeVar{} (TheType t)) =
    (,) ds <$> unifyVarToType ds var t
unifyEquality ds (Equal (TheType t) var@TypeVar{}) =
    (,) ds <$> unifyVarToType ds var t
-- Unify typevars
unifyEquality ds (Equal var1 var2) = case (ds !? var1, ds !? var2) of
    (Just dom1, Just dom2)
        | dom1 `isSubset` dom2 -> pure
            (Map.insert var2 dom1 ds, Map.empty)
        | dom2 `isSubset` dom1 -> pure
            (Map.insert var1 dom2 ds, Map.empty)
        | otherwise -> throwError (CantUnify var1 var2)
    (Nothing, _      ) -> throwError (UnboundVariable var1)
    (_      , Nothing) -> throwError (UnboundVariable var2)

unifyVarToType :: Domains -> TypeVar -> KType -> UnifyResult Substitution
unifyVarToType ds tvar t = do
    case Map.lookup tvar ds of
        Nothing -> throwError (CantUnify tvar (TheType t))
        Just ts ->
            unless (t `elem` ts) (throwError (CantUnify tvar (TheType t)))
    return (Map.singleton tvar t)

isSubset :: (Ord a, Eq a) => Set a -> Set a -> Bool
isSubset sub super = Set.difference sub super == Set.empty
