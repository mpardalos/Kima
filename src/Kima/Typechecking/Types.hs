module Kima.Typechecking.Types(
    EqConstraint, Constraint(..), (=#=), SomeConstraint(..),
    WhichConstraints(..),
    ConstraintSet, SomeConstraintSet, EqConstraintSet,
    Substitution, TypeVar(..), TVarAST, TVarProgram, TVarName,
    TypeCtx
) where

import           Data.Kind hiding (Constraint)
import           Data.List
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import qualified Data.Set as Set

import           Kima.AST
import           Kima.KimaTypes

-------------------------------- Constraints ---------------------------------------
type EqConstraint = Constraint 'OnlyEq

type ConstraintSet a = [Constraint a] 
type SomeConstraintSet = [SomeConstraint] 
type EqConstraintSet = ConstraintSet 'OnlyEq

data SomeConstraint = forall a. SomeConstraint { unpackSomeConstraint :: Constraint a }
data WhichConstraints = OnlyEq | OnlyOneOf
data Constraint :: WhichConstraints -> Type where
    Equal :: TypeVar -> TypeVar -> Constraint 'OnlyEq
    IsOneOf :: TypeVar -> Set KType -> Constraint 'OnlyOneOf
deriving instance Eq (Constraint a)
deriving instance Ord (Constraint a)

(=#=) :: TypeVar -> TypeVar -> EqConstraint
(=#=) = Equal

-- extractEqual :: Constraint a -> Maybe (Constraint 'OnlyEq)
-- extractEqual IsOneOf{}  = Nothing
-- -- Ew, but at least it's not exported.
-- -- There has to be some way to convice GHC that a matching an Equal refines the
-- -- type parameter to an OnlyEq
-- extractEqual eq@Equal{} = Just (unsafeCoerce eq) 

-- pattern AsEquality :: Constraint 'OnlyEq -> Constraint a
-- pattern AsEquality c <- (extractEqual -> Just c)
-----------------------------------------------------------------------------------

-- For now, when a name is declared, it has to have an associated type, so this
-- type is OK. If type inference is implemented to any degree, this will have to
-- be changed to map to a [TypeVar]
type TypeCtx = Map DesugaredName (Set KType)

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
instance Show (Constraint a) where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2
    show (IsOneOf t ts) = show t <> " ∈ {" ++ intercalate "," (show <$> Set.toList ts) ++ "}"

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)

instance Show SomeConstraint where
    show (SomeConstraint c) = show c

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)


instance Show TypeVar where
    show ( TypeVar         th                  ) = "@" <> show th
    show ( TheType         t                   ) = "#" <> show t
    show ( FuncTVar        (Signature args rt) ) = "(" <> intercalate ", " (show <$> args) <> ") -> " <> show rt
    show ( ApplicationTVar callee args         ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

