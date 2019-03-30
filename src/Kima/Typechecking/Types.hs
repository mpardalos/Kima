module Kima.Typechecking.Types
    ( (=#=)
    , Binding(..)
    , mapTypes
    , Domains
    , EqConstraintSet
    , Mutability(..)
    , Substitution
    , TVarAST
    , TVarIdentifier
    , TVarProgram
    , TypeAnnotatedAST
    , TypeAnnotatedProgram
    , TypecheckingError(..)
    , TypeCtx(..)
    , addType
    , addBinding
    , addBindings
    , TypeVar(..)
    , EqConstraint(..)
    )
where

import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map as Map
import           Data.Set                       ( Set )
import           Data.Text.Prettyprint.Doc
import           GHC.Generics

import           Kima.AST
import           Kima.KimaTypes

data TypecheckingError = AmbiguousVariable TypeVar [KType]
                       | AssignToConst (Identifier 'NoAnnotation)
                       | CantUnify TypeVar TypeVar
                       | CantUnifyCall TypeVar [TypeVar]
                       | MultipleSolutions TypeVar [KType]
                       | NameShadowed Name
                       | NoSolution TypeVar
                       | TypeResolutionError TypeExpr
                       | UnexpectedBuiltin BuiltinName
                       | UnboundName TVarIdentifier
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

data TypeCtx = TypeCtx {
    typeBindings :: Map TypeName KType,
    bindings :: Map (Identifier 'NoAnnotation) Binding
}

addType :: TypeName -> KType -> TypeCtx -> TypeCtx
addType name t ctx@TypeCtx { typeBindings } =
    ctx { typeBindings = Map.insert name t typeBindings }

addBinding :: Identifier 'NoAnnotation -> Binding -> TypeCtx -> TypeCtx
addBinding n b ctx@TypeCtx { bindings } =
    ctx { bindings = Map.insert n b bindings }

addBindings :: Map (Identifier 'NoAnnotation) Binding -> TypeCtx -> TypeCtx
addBindings newBindings ctx@TypeCtx { bindings } =
    ctx { bindings = bindings <> newBindings }

type Substitution = Map TypeVar KType

-- Types we have to solve for
data TypeVar = TypeVar Int
             | TheType KType
             | ApplicationTVar TypeVar [TypeVar]
    deriving (Eq, Ord, Generic)

type TVarIdentifier     = Identifier ('Annotation TypeVar)

type TVarAST (p :: ASTPart) = AST p 'NoSugar ('Annotation TypeVar) KType
type TVarProgram        = TVarAST 'Module

---------------------- Show -----------------------------
instance Show EqConstraint where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)

instance Show TypeVar where
    show ( TypeVar         th                  ) = "@" <> show th
    show ( TheType         t                   ) = "#" <> show t
    show ( ApplicationTVar callee args         ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Pretty TypeVar where
    pretty = viaShow

instance Pretty TypecheckingError where
    pretty (AmbiguousVariable var types) = 
        "Typevar" <+> pretty var <+> "is ambiguous. Available types:" 
        <> line 
        <> indent 4 (bulletList types) 
    pretty (AssignToConst name         ) = 
        "Assigned to constant" <+> pretty name
    pretty (CantUnify l r              ) = 
        "Can't unify" <+> pretty l <+> pretty r
    pretty (CantUnifyCall callee args  ) = 
        "Can't unify call to" <+> pretty callee
        <+> "with args" <+> tupled (pretty <$> args)
    pretty (MultipleSolutions var sols ) = 
        "Typevar" <+> pretty var <+> "has multiple solutions:"
        <> line
        <> indent 4 (bulletList sols)
    pretty (NameShadowed        name ) =
        "Illegal shadowing of" <+> pretty name
    pretty (NoSolution          var  ) =
        "Typevar" <+> pretty var <+> "has no solution"
    pretty (TypeResolutionError expr ) =
        "Can't resolve type" <+> pretty expr
    pretty (UnexpectedBuiltin name) =
        "Got builtin" <+> pretty name <+> "where a normal name was expected"
    pretty (UnboundName         name ) =
        "Reference to unbound name" <+> pretty name

bulletList :: Pretty a => [a] -> Doc ann
bulletList = vsep . fmap (("•" <+>) . pretty)
