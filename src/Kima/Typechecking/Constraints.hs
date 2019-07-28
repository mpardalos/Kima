module Kima.Typechecking.Constraints where

import Data.Map
import Data.Set
import Data.List
import Kima.AST.Names
import Kima.AST.Kinds
import Kima.TypeVars
import Kima.KimaTypes

data EqConstraint = Equal TypeVar TypeVar deriving (Eq, Ord)
type EqConstraintSet = [EqConstraint]

type Domains = Map TypeVar (Set KType)

-- ASTs with typevars
type TVarIdentifier = Identifier ('Annotation TypeVar)
type TVarName       = AnnotatedName ('Annotation TypeVar)

(=#=) :: TypeVar -> TypeVar -> EqConstraint
(=#=) = Equal

type Substitution = Map TypeVar KType

instance Show EqConstraint where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2

    showList [] = ("{}" <>)
    showList constraints = (intercalate "\n" (show <$> constraints) <>)
