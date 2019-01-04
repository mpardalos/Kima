{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.ConstraintSolving where

import           Control.Monad.Except
import           Kima.Control.Monad.State.Extended
import           Data.Foldable
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Kima.KimaTypes
import           Kima.Typechecking.Types


unify :: EqConstraintSet -> Domains -> Either TypecheckingError Substitution
unify cs = evalStateT (traverse_ unifyEquality cs >> extractSubstitution)

instance Monad m => MonadDomainState (StateT Domains m) where
    getDomains = get
    setDomain var d = modify (Map.insert var d)

type MonadUnify m = (MonadDomainState m, MonadError TypecheckingError m)
class MonadDomainState m where
    getDomains :: m Domains
    setDomain :: TypeVar -> Set KType -> m ()

domainOf :: MonadUnify m => TypeVar -> m (Set KType)
domainOf tvar@TypeVar{} = Map.lookup tvar <$> getDomains >>= \case
    Just domain -> pure domain
    Nothing     -> throwError (UnsetDomain tvar)
domainOf (     TheType t                  ) = pure [t]
domainOf tvar@(ApplicationTVar callee args) = do
    argDomains   <- toList <$> domainOf `traverse` args
    calleeDomain <- toList <$> domainOf callee
    case filter (matches argDomains) calleeDomain of
        [t@(KFunc (Signature _ rt))] -> unifyVarToType callee t *> pure [rt]
        [t] -> throwError (CantUnify (TheType t) callee)
        []                           -> throwError (CantUnifyCall tvar args)
        ts                           -> throwError (AmbiguousVariable tvar ts)
  where
    matches :: [Set KType] -> KType -> Bool
    matches argDomains (KFunc (Signature args' _)) =
        all id (zipWith elem args' argDomains)
    matches _ _ = False

extractSubstitution :: forall m . MonadUnify m => m Substitution
extractSubstitution = Map.traverseWithKey extractSubstitution1 =<< getDomains
  where
    extractSubstitution1 _    (toList -> [t]) = pure t
    extractSubstitution1 tvar (toList -> [] ) = throwError (NoSolution tvar)
    extractSubstitution1 tvar (toList -> ts) =
        throwError (MultipleSolutions tvar ts)

-- | Try to unify two typevars under a given set of domains.
-- | Return the updated domains and a the substitution that unifies the two 
-- | typevars. 
unifyEquality :: MonadUnify m => EqConstraint -> m ()
-- Unify concrete types
unifyEquality (Equal (TheType t1) (TheType t2))
    | t1 == t2  = pure ()
    | otherwise = throwError (CantUnify (TheType t1) (TheType t2))
-- Unify vars to concrete types
unifyEquality (Equal var         (TheType t)) = unifyVarToType var t
unifyEquality (Equal (TheType t) var        ) = unifyVarToType var t
-- Unify typevars
unifyEquality (Equal var1        var2       ) = do
    dom1 <- domainOf var1
    dom2 <- domainOf var2
    case () of
        () | dom1 `isSubset` dom2 -> setDomain var2 dom1
           | dom2 `isSubset` dom1 -> setDomain var1 dom2
           | otherwise            -> throwError (CantUnify var1 var2)

unifyVarToType :: MonadUnify m => TypeVar -> KType -> m ()
unifyVarToType tvar KUnit = do
    domain <- domainOf tvar
    when (domain == [KUnit]) (setDomain tvar [KUnit])
unifyVarToType tvar t = do
    domain <- domainOf tvar
    unless (t `elem` domain) (throwError (CantUnify tvar (TheType t)))
    setDomain tvar [t]

isSubset :: (Ord a, Eq a) => Set a -> Set a -> Bool
isSubset sub super = Set.difference sub super == []
