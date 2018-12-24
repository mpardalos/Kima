module Kima.Typechecking.Types(
    Constraint, EqConstraint, ReducedConstraint(..), (=#=), 
    WhichConstraints(..), pattern AsEquality, 
    ReducedConstraintSet, ConstraintSet, EqConstraintSet,
    Substitution, TypeVar(..), TVarAST, TVarProgram, TVarName
) where

import           Unsafe.Coerce
import           Data.Kind hiding (Constraint)
import           Data.List
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import qualified Data.Set as Set

import           Kima.AST
import           Kima.KimaTypes

-------------------------------- Constraints ---------------------------------------
type Constraint = ReducedConstraint 'All
type EqConstraint = ReducedConstraint 'OnlyEq

data WhichConstraints = All | OnlyEq
data ReducedConstraint :: WhichConstraints -> Type where
    Equal :: TypeVar -> TypeVar -> ReducedConstraint a
    IsOneOf :: TypeVar -> Set KType -> ReducedConstraint 'All
    Failure :: ReducedConstraint 'All
deriving instance Eq (ReducedConstraint a)
deriving instance Ord (ReducedConstraint a)

(=#=) :: TypeVar -> TypeVar -> ReducedConstraint a
(=#=) = Equal

extractEqual :: ReducedConstraint a -> Maybe (ReducedConstraint 'OnlyEq)
extractEqual IsOneOf{}  = Nothing
extractEqual Failure = Nothing
-- Ew, but at least it's not exported.
-- There has to be some way to convice GHC that a matching an Equal refines the
-- type parameter to an OnlyEq
extractEqual eq@Equal{} = Just (unsafeCoerce eq) 

pattern AsEquality :: ReducedConstraint 'OnlyEq -> ReducedConstraint a
pattern AsEquality c <- (extractEqual -> Just c)

-----------------------------------------------------------------------------------

-------------------------------- Constraint Sets ----------------------------------
type ReducedConstraintSet a = [ReducedConstraint a] 

type ConstraintSet = ReducedConstraintSet 'All
type EqConstraintSet = ReducedConstraintSet 'OnlyEq
-----------------------------------------------------------------------------------

type Substitution = Map TypeVar KType

-- Types we have to solve for
data TypeVar = TypeVar Int
             | TheType KType
             | FuncTVar (Signature TypeVar)
             | ApplicationTVar TypeVar [TypeVar]
    deriving (Eq, Ord)

type TVarName         = GenericName ('Just TypeVar) 'True

type TVarAST p          = AST p 'NoSugar TVarName 'Nothing
type TVarProgram        = TVarAST 'TopLevel

---------------------- Show -----------------------------
instance Show (ReducedConstraint a) where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2
    show (IsOneOf t ts) = show t <> " ∈ {" ++ intercalate "," (show <$> Set.toList ts) ++ "}"
    show Failure = "Failure"

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)


instance Show TypeVar where
    show ( TypeVar         th                  ) = "@" <> show th
    show ( TheType         t                   ) = "#" <> show t
    show ( FuncTVar        (Signature args rt) ) = "(" <> intercalate ", " (show <$> args) <> ") -> " <> show rt
    show ( ApplicationTVar callee args         ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

