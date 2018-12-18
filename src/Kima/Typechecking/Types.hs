module Kima.Typechecking.Types where

import           Data.Coerce
import           Data.List
import           Data.Map
import           GHC.Exts                       ( IsList(..) )

import           Kima.AST
import           Kima.KimaTypes

data Constraint = Equal TypeVar TypeVar
                | IsOneOf TypeVar [KType]
                | Failure
    deriving (Eq, Ord)
(=#=) = Equal

newtype ConstraintSet = ConstraintSet [Constraint]
    deriving (Semigroup, Monoid)

-- Types we have to solve for
data TypeVar = TypeHole Int
             | TheType KType
             | FuncHole (Signature TypeVar)
             | ApplicationHole TypeVar [TypeVar]
    deriving (Eq, Ord)

type HoleMap          = Map DesugaredName TypeVar
type HoleSubstitution = Map TypeVar      KType
type HoleName         = GenericName ('Just TypeVar) 'True

type HoleAST p          = AST p 'NoSugar HoleName 'Nothing
type HoleProgram        = HoleAST 'TopLevel

-------------------- Instances -------------------------
instance IsList ConstraintSet where
    type Item ConstraintSet = Constraint
    fromList = coerce
    toList = coerce

---------------------- Show -----------------------------
instance Show ConstraintSet where
    show (ConstraintSet []) = "{}"
    show (ConstraintSet constraints) = intercalate "\n" (show <$> constraints)

instance Show Constraint where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2 
    show (IsOneOf t ts) = show t <> " ∈ {" ++ intercalate "," (show <$> ts) ++ "}"
    show Failure = "Failure"

instance Show TypeVar where
    show ( TypeHole        th                  ) = "@" <> show th
    show ( TheType         t                   ) = "#" <> show t
    show ( FuncHole        (Signature args rt) ) = "(" <> intercalate ", " (show <$> args) <> ") -> " <> show rt
    show ( ApplicationHole callee args         ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

