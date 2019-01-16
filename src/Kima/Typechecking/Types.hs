module Kima.Typechecking.Types
    ( (=#=)
    , AnnotatedTVarProgram
    , AnnotatedTVarAST
    , Binding(..)
    , mapTypes
    , Domains
    , EqConstraintSet
    , Mutability(..)
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
import           GHC.Generics

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
    deriving (Eq, Show)

-------------------------------- Constraints ---------------------------------------
data EqConstraint = Equal TypeVar TypeVar deriving (Eq, Ord)
type EqConstraintSet = [EqConstraint]

type Domains = Map TypeVar (Set KType)

(=#=) :: TypeVar -> TypeVar -> EqConstraint
(=#=) = Equal
-----------------------------------------------------------------------------------

data Binding = Binding {
    mutability :: Mutability,
    types :: Set KType
} deriving (Eq, Ord, Show, Generic)

mapTypes :: (Set KType -> Set KType) -> Binding -> Binding
mapTypes f b@Binding { types } = b { types = f types}

data Mutability = Constant | Variable
    deriving (Eq, Ord, Show, Generic)
instance Semigroup Mutability where
    Constant <> _ = Constant
    _ <> Constant = Constant
    _ <> _        = Variable
instance Semigroup Binding where
    (Binding mutL typesL) <> (Binding mutR typesR) = 
        Binding (mutL <> mutR) (typesL <> typesR)


-- For now, when a name is declared, it has to have an associated type, so this
-- type is OK. If type inference is implemented to any degree, this will have to
-- be changed to map to a [TypeVar]
type TypeCtx = Map DesugaredName Binding

type Substitution = Map TypeVar KType

-- Types we have to solve for
data TypeVar = TypeVar Int
             | TheType KType
             | ApplicationTVar TypeVar [TypeVar]
    deriving (Eq, Ord, Generic)

type TVarName         = GenericName ('Just TypeVar) 'True

type TVarAST p          = AST p 'NoSugar TVarName 'Nothing
type TVarProgram        = TVarAST 'Module

type AnnotatedTVarAST p = AST p 'NoSugar TVarName ('Just KType)
type AnnotatedTVarProgram = AnnotatedTVarAST 'Module

type TypeAnnotatedAST p = AST p 'NoSugar DesugaredName ('Just KType)
type TypeAnnotatedProgram = TypeAnnotatedAST 'Module
---------------------- Show -----------------------------
instance Show EqConstraint where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)

instance Show TypeVar where
    show ( TypeVar         th                  ) = "@" <> show th
    show ( TheType         t                   ) = "#" <> show t
    show ( ApplicationTVar callee args         ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

