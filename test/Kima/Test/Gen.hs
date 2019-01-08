{-# OPTIONS -Wno-orphans #-}
module Kima.Test.Gen where

import           Test.QuickCheck
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import           Kima.KimaTypes
import           Kima.AST
import           Kima.Typechecking.Types
import           Generic.Random

{-# ANN domainsWithValues ("HLint: ignore Use <$>" :: String) #-}
-- | Domains where the sets of types are drawn from the given generator
domainsWithValues :: Gen [Set KType] -> Gen Domains
domainsWithValues valueGen = do
    keys   <- listOf (TypeVar <$> arbitrarySizedNatural)
    values <- valueGen
    return (Map.fromList (zip keys values))

{-# ANN nonEmptyDomainsWithValues ("HLint: ignore Use <$>" :: String) #-}
-- | Domains where the sets of types are drawn from the given generator
nonEmptyDomainsWithValues :: Gen [Set KType] -> Gen Domains
nonEmptyDomainsWithValues valueGen = do
    keys   <- listOf1 (TypeVar <$> arbitrarySizedNatural)
    values <- valueGen
    return (Map.fromList (zip keys values))

arbitraryDomains :: Gen Domains
arbitraryDomains =
    domainsWithValues (listOf (Set.fromList <$> listOf (arbitrary @KType)))

arbitrarySolvedDomains :: Gen Domains
arbitrarySolvedDomains =
    domainsWithValues (listOf (Set.singleton <$> arbitrary @KType))

arbitraryUnsolvedDomains :: Gen Domains
arbitraryUnsolvedDomains = nonEmptyDomainsWithValues $ listOf1 $ do
    t1 <- arbitrary @KType
    t2 <- arbitrary @KType `suchThat` (/= t1)
    return [t1, t2]

disjointSets :: forall a . Ord a => Gen a -> Gen (Set a, Set a)
disjointSets gen = do
    set1 <- Set.fromList <$> listOf gen
    set2 <- Set.fromList <$> listOf gen
    let set1' = set1 \\ set2
    let set2' = set2 \\ set1
    return (set1', set2')

arbitrarySingleType :: Gen KType
arbitrarySingleType = baseCase

instance Arbitrary KType where
    arbitrary = sized $ \case
        n | n <= 1 -> arbitrarySingleType
        n          -> frequency
            [ (3, arbitrarySingleType)
            -- Shrink the type parameter quickly because otherwise it keeps
            -- recursing and making huge signature types which take forever
            , (1, KFunc <$> resize (n `div` 3) (arbitrary @(Signature KType)))
            ]

    shrink (KFunc sig) = KFunc <$> shrink sig
    shrink _           = []

instance Arbitrary TypeVar where
    arbitrary = genericArbitrary'
        $ (2 :: W "TypeVar") 
        % (2 :: W "TheType") 
        % (1 :: W "ApplicationTVar") 
        % ()

instance Arbitrary t => Arbitrary (Signature t) where
    arbitrary = genericArbitraryU
    shrink (Signature args rt) = Signature <$> shrink args <*> shrink rt

instance Arbitrary Literal where
    arbitrary = genericArbitraryU
    shrink = genericShrink

instance Arbitrary a => Arbitrary (Binary a) where
    arbitrary = genericArbitraryU

instance Arbitrary a => Arbitrary (Unary a) where
    arbitrary = genericArbitraryU
