{-# LANGUAGE OverloadedLists #-}
module Kima.Test.ConstraintSolver where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Kima.Test.Gen
import           Data.Either
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Map                      ((!?))
import           Kima.Typechecking.ConstraintSolving
import           Kima.Typechecking.Types
import           Kima.KimaTypes
import           Control.Monad.State
-- import qualified Data.Map as Map

spec :: Spec
spec = describe "Constraint Solver" $ do
    it "Extracts empty substitution"
        $          extractSubstitution `withDomains` []
        `shouldBe` Right []

    prop "Extracts correct substitution for valid solution" $
        verboseShrinking $
        forAll arbitrarySolvedDomains $ \ds -> 
            extractSubstitution 
            `withDomains` ds 
            `shouldBe` Right ((!! 0) <$> (Set.toList <$> ds))

    prop "Does not extract substitution for invalid solution" $ 
        forAll arbitraryUnsolvedDomains $ \ds ->                 
            extractSubstitution 
            `withDomains` ds 
            `shouldSatisfy` isLeft

    prop "Unify var to type completely constraints type" $ 
        forAll arbitraryDomains                    $ \ds    ->
        forAll (TypeVar <$> arbitrarySizedNatural) $ \tvar  ->
        forAll (arbitrary @KType)                  $ \ktype ->
            (unifyVarToType tvar ktype >> get)
            `withDomains` Map.alter (\case
                Just ts -> Just ([ktype] <> ts)
                Nothing -> Just [ktype]) tvar ds
            `shouldSatisfy` \case
                Right ds' -> ds' !? tvar == Just [ktype]
                Left{} -> False

    prop "Draws correct domain for simple vars" $ 
        forAll (TypeVar <$> arbitrarySizedNatural)  $ \tvar   ->
        forAll (Set.fromList <$> listOf1 arbitrary) $ \domain -> 
            domainOf tvar 
            `withDomains` [(tvar, domain)] 
            `shouldBe` Right domain

    prop "Draws correct domain for simple vars" $ 
        forAll (TypeVar <$> arbitrarySizedNatural)  $ \tvar   ->
        forAll arbitraryDomains                     $ \domains -> 
        forAll (Set.fromList <$> listOf1 arbitrary) $ \domain -> 
            domainOf tvar 
            `withDomains` Map.insert tvar domain domains
            `shouldBe`    Right domain

    prop "Domain of call to non-function type is an error" $ 
        forAll (arbitrary @ KType)    $ \callee ->
        forAll (arbitrary @[TypeVar]) $ \args   ->
        forAll arbitraryDomains       $ \ds     -> 
            domainOf (ApplicationTVar (TheType callee) args) 
            `withDomains`   ds 
            `shouldSatisfy` isLeft

    prop "Domain of call to non-function typevar is an error" $ 
        forAll (Set.fromList <$> listOf arbitrarySingleType)  $ \calleeDomain ->
        forAll (TypeVar <$> arbitrarySizedNatural)            $ \calleeVar    ->
        forAll (arbitrary @[TypeVar])                         $ \args         ->
        forAll arbitraryDomains                               $ \ds           -> 
            domainOf (ApplicationTVar calleeVar args) 
            `withDomains`   Map.insert calleeVar calleeDomain ds
            `shouldSatisfy` isLeft

    prop "Domain of call with wrong argument count is an error" $ 
        forAll (arbitrary @[KType])   $ \expectedArgs ->
        forAll (arbitrary @KType)     $ \rt           ->
        forAll (arbitrary @[TypeVar] 
        `suchThat` ((/=length expectedArgs) . length)) 
                                      $ \args         ->
        forAll arbitraryDomains       $ \ds           -> 
            domainOf 
                (ApplicationTVar (TheType (KFunc (expectedArgs $-> rt))) args) 
            `withDomains`   ds 
            `shouldSatisfy` isLeft

    prop "Different constant types don't unify" $
        forAll arbitraryDomains                     $ \ds ->
        forAll (arbitrary @KType)                   $ \t1 ->
        forAll (arbitrary @KType `suchThat` (/=t1)) $ \t2 ->
            unifyEquality (TheType t1 =#= TheType t2) 
            `withDomains` ds
            `shouldSatisfy` isLeft

    prop "Disjoint domains don't unify" $
        forAll arbitraryDomains                         $ \ds ->
        forAll (disjointSets (arbitrary @KType))        $ \(d1, d2) ->
        forAll (TypeVar <$> arbitrarySizedNatural)      $ \tvar1 ->
        forAll ((TypeVar <$> arbitrarySizedNatural)
               `suchThat` (/=tvar1))                    $ \tvar2 ->
            (unifyEquality (tvar1 =#= tvar2) >> get)
            `withDomains` Map.insert tvar1 d1 
                         (Map.insert tvar2 d2 ds)
            `shouldSatisfy` isLeft
        
withDomains
    :: StateT Domains (Either TypecheckingError) a
    -> Domains
    -> Either TypecheckingError a
withDomains = evalStateT