{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -Wno-orphans #-}
module Kima.Test.DomainCalculation where

import Control.Monad.State
import Data.Either
import qualified Data.Set as Set
import qualified Data.Map as Map
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
    prop "Doesn't allow assign to constant" $ 
        forAll arbitrary $ \name -> 
        forAll arbitrary $ \(getNonEmpty -> (Set.fromList -> types)) -> 
        forAll arbitraryExprWithTypeCtx $ \(expr, typeCtx) ->
        calculateDomains (Assign name expr)
        `withTypeCtx` (
            [(deTypeAnnotate name, Binding Constant types)]
            <> typeCtx)
        `shouldSatisfy` isLeft

    it "Allows assign to variable" $
        calculateDomains (Assign 
            (TypedName "a" (TypeVar 1)) 
            (LiteralE (IntExpr 5)))
        `withTypeCtx` [(Name "a", Binding Variable [KFloat])]
        `shouldSatisfy` isRight
    
withTypeCtx
    :: StateT TypeCtx (Either TypecheckingError) a
    -> TypeCtx
    -> Either TypecheckingError a
withTypeCtx = evalStateT

arbitraryExprWithTypeCtx
    :: Arbitrary (AST p s TVarName t)
    => Gen (AST p s TVarName t, TypeCtx)
arbitraryExprWithTypeCtx = do
    ast <- arbitrary
    let freeVars = deTypeAnnotate <$> getFreeVars ast
    let typeCtx = Map.fromList (zip 
            freeVars 
            [ Binding Constant [KFloat]
            , Binding Variable [KString, KInt]
            ])
    return (ast, typeCtx)
