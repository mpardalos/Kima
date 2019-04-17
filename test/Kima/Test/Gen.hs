{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -Wno-orphans #-}
module Kima.Test.Gen where

import           Test.QuickCheck
import           Control.Monad
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

arbitraryProductType :: Gen (KType, [(String, KType)])
arbitraryProductType = do
    name       <- arbitrary
    typeFields <- scale (`div` 3) arbitrary
    return (KUserType name typeFields, typeFields)

arbitraryWriteIdentifier :: Arbitrary ident => Gen (WriteAccess ident)
arbitraryWriteIdentifier = WriteAccess <$> arbitrary <*> pure []

instance Arbitrary KType where
    arbitrary = sized $ \case
        n | n <= 1 -> arbitrarySingleType
        n          -> frequency
            [ (1, arbitrarySingleType)
            , (2, fst <$> arbitraryProductType)
            -- Shrink the type parameter quickly because otherwise it keeps
            -- recursing and making huge signature types which take forever
            , (2, KFunc <$> resize (n `div` 3) (arbitrary @(Signature KType)))
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

instance Arbitrary ident => Arbitrary (WriteAccess ident) where
    arbitrary = WriteAccess <$> arbitrary <*> arbitrary

instance (Arbitrary t,
          Arbitrary (Identifier idAnn),
          Arbitrary (AnnotatedName idAnn)) =>
         Arbitrary (AST 'Expr 'NoSugar idAnn t) where
    arbitrary = oneof
        [ LiteralE <$> arbitrary
        , IdentifierE <$> arbitrary
        , FuncExpr <$> arbitrary <*> arbitrary <*> arbitrary
        , Call <$> arbitrary <*> arbitrary
        ]

    shrink (LiteralE lit) = LiteralE <$> shrink lit
    shrink (IdentifierE name) = IdentifierE <$> shrink name
    shrink (FuncExpr args rt body) = FuncExpr <$> shrink args <*> shrink rt <*> shrink body
    shrink (Call callee args) = Call <$> shrink callee <*> shrink args

instance (Arbitrary t,
          Arbitrary (Identifier idAnn),
          Arbitrary (AnnotatedName idAnn)) =>
         Arbitrary (AST 'Stmt 'NoSugar idAnn t) where
    arbitrary = oneof
        [ ExprStmt <$> arbitrary
        , Block    <$> scale (`div` 2) arbitrary
        , While    <$> (WhileStmt <$> arbitrary <*> arbitrary)
        , If       <$> (IfStmt <$> arbitrary <*> arbitrary <*> arbitrary)
        , Assign   <$> arbitrary <*> arbitrary
        , Var      <$> arbitrary <*> arbitrary <*> arbitrary
        , Let      <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary BuiltinName where
    arbitrary = genericArbitraryU

instance Arbitrary (Identifier 'NoAnnotation) where
    arbitrary = oneof 
        [ Identifier <$> (getNonEmpty <$> arbitrary)
        , Builtin <$> arbitrary
        ]
    shrink = shrinkUntypedName

instance Arbitrary t => Arbitrary (Identifier ('Annotation t)) where
    arbitrary = typeAnnotate 
        <$> arbitrary @t
        <*> arbitrary @(Identifier 'NoAnnotation)
    shrink (TIdentifier n t)
        | length n > 1 = TIdentifier <$> shrink n <*> shrink t
        | otherwise    = TIdentifier n <$> shrink t
    shrink (TBuiltin n t) = join 
        [ TBuiltin n <$> shrink t
        , TIdentifier (show n) <$> shrink t
        ]
    shrink (TAccessor n t) = join 
        [ TAccessor <$> shrink n <*> shrink t 
        , TIdentifier (show n) <$> shrink t
        ]

instance Arbitrary (AnnotatedName 'NoAnnotation) where
    arbitrary = Name <$> arbitrary

instance Arbitrary t => Arbitrary (AnnotatedName ('Annotation t)) where
    arbitrary = TName <$> arbitrary <*> arbitrary


shrinkUntypedName :: Identifier 'NoAnnotation -> [Identifier 'NoAnnotation]
shrinkUntypedName (Identifier    n)
    | length n > 1 = Identifier <$> shrink n
    | otherwise    = []
shrinkUntypedName (Accessor n) 
    | length n > 1 = join [Identifier <$> shrink n, Accessor <$> shrink n]
    | otherwise    = []
-- Base names are smaller than builtins 
shrinkUntypedName (Builtin n) = [Identifier (show n)]
