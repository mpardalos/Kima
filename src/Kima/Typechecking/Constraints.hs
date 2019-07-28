module Kima.Typechecking.Constraints where

import Data.Map
import Data.Set
import Data.List
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Kima.AST
import Kima.KimaTypes

data EqConstraint = Equal TypeVar TypeVar deriving (Eq, Ord)
type EqConstraintSet = [EqConstraint]

type Domains = Map TypeVar (Set KType)

(=#=) :: TypeVar -> TypeVar -> EqConstraint
(=#=) = Equal

type Substitution = Map TypeVar KType

-- Types we have to solve for
data TypeVar = TypeVar Int
             | TheType KType
             | ApplicationTVar TypeVar [TypeVar]
    deriving (Eq, Ord, Generic)

-- ASTs with typevars
type TVarIdentifier = Identifier ('Annotation TypeVar)
type TVarName       = AnnotatedName ('Annotation TypeVar)

-- | AST tag for the AST with typevars stage
data TVars
instance ASTTag TVars where
    type TagSugar TVars = 'NoSugar
    type NameAnnotation TVars = 'Annotation TypeVar
    type FreeAnnotation TVars = KType

instance Show EqConstraint where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)

instance Show TypeVar where
    show ( TypeVar         th          ) = "@" <> show th
    show ( TheType         t           ) = "#" <> show t
    show ( ApplicationTVar callee args ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Pretty TypeVar where
    pretty = viaShow
