module Kima.Typechecking.Types
    ( (=#=)
    , AnnotatedTVarProgram 
    , AnnotatedTVarAST 
    , Constraint(..)
    , ConstraintSet
    , EqConstraintSet
    , SomeConstraint(..)
    , SomeConstraintSet
    , Substitution
    , TVarAST
    , TVarName
    , TVarProgram
    , TypeAnnotatedAST
    , TypeAnnotatedProgram
    , TypecheckingError(..)
    , TypeCtx
    , TypeVar(..)
    , WhichConstraints(..)
    , EqConstraint
    )
where

import           Data.Kind               hiding ( Constraint )
import           Data.List
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Kima.AST
import           Kima.KimaTypes

data TypecheckingError = AmbiguousVariable TypeVar [KType]
                       | CantUnify TypeVar TypeVar
                       | CantUnifyCall TypeVar [TypeVar]
                       | DomainMismatch
                       | MultipleSolutions TypeVar [KType]
                       | NameShadowed TVarName
                       | NoSolution TypeVar
                       | TypeResolutionError TypeExpr
                       | UnboundName TVarName
                       | UnsetDomain TypeVar
    deriving Show


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
-----------------------------------------------------------------------------------

-- For now, when a name is declared, it has to have an associated type, so this
-- type is OK. If type inference is implemented to any degree, this will have to
-- be changed to map to a [TypeVar]
type TypeCtx = Map DesugaredName (Set KType)

type Substitution = Map TypeVar KType

-- Types we have to solve for
data TypeVar = TypeVar Int
             | TheType KType
             | ApplicationTVar TypeVar [TypeVar]
    deriving (Eq, Ord)

type TVarName         = GenericName ('Just TypeVar) 'True

type TVarAST p          = AST p 'NoSugar TVarName 'Nothing
type TVarProgram        = TVarAST 'TopLevel

type AnnotatedTVarAST p = AST p 'NoSugar TVarName ('Just KType)
type AnnotatedTVarProgram = AnnotatedTVarAST 'TopLevel

type TypeAnnotatedAST p = AST p 'NoSugar DesugaredName ('Just KType)
type TypeAnnotatedProgram = TypeAnnotatedAST 'TopLevel

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
    show ( ApplicationTVar callee args         ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

