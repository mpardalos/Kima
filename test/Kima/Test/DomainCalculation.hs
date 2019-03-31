{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -Wno-orphans #-}
module Kima.Test.DomainCalculation where

import Control.Monad.State
import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import Kima.Typechecking.DomainCalculation
import Kima.Typechecking.Types
import Kima.KimaTypes
import Kima.AST
import Kima.Test.Gen()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Domain Calculator" $ do
    prop "Doesn't allow assign to constant" $ 
        forAll arbitrary $ \name ->
        calculateDomains (Assign (WriteAccess (name :| [])) (LiteralE (IntExpr 5)))
        `withTypeCtx` TypeCtx
            []
            [(deTypeAnnotate name, Binding Constant [KInt])]
        `shouldSatisfy` isLeft

    it "Allows assign to variable" $
        calculateDomains (Assign 
            (WriteAccess ((TIdentifier "a" (TypeVar 1)) :| []))
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
