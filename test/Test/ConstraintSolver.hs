{-# LANGUAGE OverloadedLists #-}
module Test.ConstraintSolver where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Kima.Typechecking.ConstraintSolving
import           Kima.Typechecking.Types
import           Kima.KimaTypes
import           Control.Monad.State
-- import qualified Data.Map as Map

constraintSolverSpec :: Spec
constraintSolverSpec = parallel $ describe "Constraint Solver" $ do
    it "Extracts empty substitution"
        $          (extractSubstitution `withDomains` [])
        `shouldBe` Right []
    -- prop "Extracts substitution 1" $ \ds -> 
    --     (extractSubstitution `withDomains` ds)
    --     `shouldBe` Right (_ ds)

withDomains
    :: StateT Domains (Either TypecheckingError) a
    -> Domains
    -> Either TypecheckingError a
withDomains = evalStateT
