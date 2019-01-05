{-# LANGUAGE OverloadedLists, Strict #-}
{-# OPTIONS -Wno-orphans #-}
module Test.ConstraintSolver where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Data.Either
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Kima.Typechecking.ConstraintSolving
import           Kima.Typechecking.Types
import           Kima.KimaTypes
import           Control.Monad.State
-- import qualified Data.Map as Map

constraintSolverSpec :: Spec
constraintSolverSpec = describe "Constraint Solver" $ do
    it "Extracts empty substitution"
        $          (extractSubstitution `withDomains` [])
        `shouldBe` Right []

    prop "Extracts correct substitution for valid solution"
        $ forAll arbitrarySolvedDomains
        $ \ds -> extractSubstitution `withDomains` ds `shouldBe` Right
              ((!! 0) <$> (Set.toList <$> ds))

    prop "Does not extract substitution for invalid solution"
        $ forAll arbitraryUnsolvedDomains
        $ \ds -> extractSubstitution `withDomains` ds `shouldSatisfy` isLeft

    prop "Unify var to type completely constraints type"
         (pendingWith "hangs some times")
        -- $ \(tvar, ktype) ->
        -- forAll (arbitraryDomains `suchThat` ((tvar `elem`) . Map.keys))
        --     $ \ds ->
        --           (unifyVarToType tvar ktype >> get)
        --               `withDomains`   ds
        --               `shouldSatisfy` \case
        --                                   Left{}    -> False
        --                                   Right ds' -> Map.lookup tvar ds'
        --                                       == Just [ktype]


withDomains
    :: StateT Domains (Either TypecheckingError) a
    -> Domains
    -> Either TypecheckingError a
withDomains = evalStateT

arbitraryDomains :: Gen Domains
arbitraryDomains = do
    keys   <- listOf (TypeVar <$> arbitrarySizedNatural)
    values <- listOf (Set.fromList <$> listOf (arbitrary @KType))
    pure (Map.fromList (zip keys values))

arbitrarySolvedDomains :: Gen Domains
arbitrarySolvedDomains = do
    keys   <- listOf (TypeVar <$> arbitrarySizedNatural)
    values <- listOf (Set.singleton <$> arbitrary @KType)
    pure (Map.fromList (zip keys values))

arbitraryUnsolvedDomains :: Gen Domains
arbitraryUnsolvedDomains = do
    keys   <- listOf1 (TypeVar <$> arbitrarySizedNatural)
    values <- listOf1 (listOf1 (arbitrary @KType) `suchThat` ((> 1) . length))
    let values' = Set.fromList <$> values
    pure (Map.fromList (zip keys values'))


instance Arbitrary TypeVar where
    arbitrary = oneof
        [ TypeVar <$> arbitrary
        , TheType <$> arbitrary
        , ApplicationTVar <$> arbitrary <*> arbitrary
        ]

instance Arbitrary KType where
    arbitrary = sized $ \case
        n | n <= 1 -> elements [KString, KUnit, KBool, KInt, KFloat]
        n          -> oneof
            [ elements [KString, KUnit, KBool, KInt, KFloat]
            -- Shrink the type parameter quickly because otherwise it keeps
            -- recursing and making huge signature types which take forever
            , KFunc <$> resize (n `div` 2) (arbitrary @(Signature KType))
            ]

instance Arbitrary t => Arbitrary (Signature t) where
    arbitrary = Signature <$> arbitrary <*> arbitrary
