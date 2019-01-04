module Kima.Typechecking.Types
    ( (=#=)
    , AnnotatedTVarProgram
    , AnnotatedTVarAST
    , Domains
    , EqConstraintSet
    , Substitution
    , TVarAST
    , TVarName
    , TVarProgram
    , TypeAnnotatedAST
    , TypeAnnotatedProgram
    , TypecheckingError(..)
    , TypeCtx
    , TypeVar(..)
    , EqConstraint(..)
    )
where

import           Data.List
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )

import           Kima.AST
import           Kima.KimaTypes

data TypecheckingError = AmbiguousVariable TypeVar [KType]
                       | AssignToConst TVarName
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
data EqConstraint = Equal TypeVar TypeVar deriving (Eq, Ord)
type EqConstraintSet = [EqConstraint]

type Domains = Map TypeVar (Set KType)

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
instance Show EqConstraint where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)

instance Show TypeVar where
    show ( TypeVar         th                  ) = "@" <> show th
    show ( TheType         t                   ) = "#" <> show t
    show ( ApplicationTVar callee args         ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

