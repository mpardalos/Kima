{-# LANGUAGE OverloadedLists, DerivingStrategies, DeriveAnyClass #-}
module Kima.Typechecking.ConstraintSolving where

import           Control.Monad.Except
import           Kima.Control.Monad.State.Extended
import           Control.Monad.Reader.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Kima.KimaTypes
import           Kima.Typechecking.Types


-------------------------------------------- Interpreter -----------------------------------------------
unify :: SomeConstraintSet -> Either TypecheckingError Substitution
unify cs = evalStateT
    (runUnifyM
        (reduceConstraints cs >>= traverse_ unifyEquality >> extractSubstitution
        )
    )
    []
newtype UnifyM a = UnifyM { runUnifyM :: StateT Domains (Either TypecheckingError) a }
    deriving newtype (Functor, Applicative, Monad, MonadState Domains, MonadError TypecheckingError)
    deriving anyclass (MonadUnify)
instance MonadReader Domains UnifyM where
    ask = get
    local = withState
--------------------------------------------------------------------------------------------------------

type Domains = Map TypeVar (Set KType)

class MonadError TypecheckingError m => MonadUnify m where
    getDomains :: m Domains
    default getDomains :: MonadReader Domains m => m Domains
    getDomains = ask

    setDomain :: TypeVar -> Set KType -> m ()
    default setDomain :: MonadState Domains m => TypeVar -> Set KType -> m ()
    setDomain var d = modify (Map.insert var d)

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

reduceConstraints
    :: forall m . MonadUnify m => SomeConstraintSet -> m EqConstraintSet
reduceConstraints = (catMaybes <$>) . traverse go
  where
    go (SomeConstraint eqConstraint@Equal{} ) = pure @m (Just eqConstraint)
    go (SomeConstraint (IsOneOf tvar domain)) = do
        Map.lookup tvar <$> getDomains >>= \case
            Just oldDomain ->
                unless (domain `isSubset` oldDomain) (throwError DomainMismatch)
            Nothing -> pure ()
        setDomain tvar domain
        return Nothing

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
