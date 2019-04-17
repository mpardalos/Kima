{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# OPTIONS -Wno-orphans #-}
module Kima.Test.DomainCalculation where

import Control.Monad.State
import Data.Either
import Kima.Typechecking.DomainCalculation
import Kima.Typechecking.Types
import Kima.KimaTypes
import Kima.AST
import Kima.Test.Gen
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Domain Calculator" $ do
    it "Doesn't allow assign to constant" $
        let access = WriteAccess (TName "a" (TypeVar 1)) [] in
        calculateDomains (Assign access (LiteralE (IntExpr 5)))
        `withTypeCtx` TypeCtx
            []
            [("a", Binding Constant [KInt])]
        `shouldBe` Left (AssignToConst access)

    prop "Doesn't allow assign to field of constant" $
        forAll arbitraryProductType  $ \(productType, typeFields) ->
        typeFields /= [] ==>
        forAll (elements typeFields) $ \(field, _) ->
        let access = WriteAccess (TName "a" (TypeVar 1)) [TName field (TypeVar 2)] in
        calculateDomains (Assign access (LiteralE (IntExpr 5)))
        `withTypeCtx` TypeCtx
            []
            [("a", Binding Constant [productType])]
        `shouldSatisfy` isLeft

    it "Allows assign to variable" $
        calculateDomains (Assign 
            (WriteAccess (TName "a" (TypeVar 1)) [])
            (LiteralE (IntExpr 5)))
        `withTypeCtx` TypeCtx 
            []
            [(Identifier "a", Binding Variable [KFloat])]
        `shouldSatisfy` isRight
    
withTypeCtx
    :: StateT TypeCtx (Either TypecheckingError) a
    -> TypeCtx
    -> Either TypecheckingError a
withTypeCtx = evalStateT
