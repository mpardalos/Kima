module Kima.Typechecking.Types where

import           Data.Coerce
import           Data.List
import           Data.Map
import           GHC.Exts                       ( IsList(..) )

import           Kima.AST
import           Kima.KimaTypes

data Constraint = Equal TypeHole TypeHole
                | AcceptsArgs TypeHole [TypeHole]
                | IsConstant DesugaredName
                | IsVariable DesugaredName
    deriving (Eq, Ord)
(=#=) = Equal

newtype ConstraintSet = ConstraintSet [Constraint]
    deriving (Semigroup, Monoid)

-- Types we have to solve for
data TypeHole = TypeHole Int
              | TheType KType
              | FuncHole [TypeHole] TypeHole
              | ApplicationHole TypeHole [TypeHole]
    deriving (Eq, Ord)

type HoleMap          = Map DesugaredName TypeHole
type HoleSubstitution = Map TypeHole      KType

type HoleName  = GenericName ('Just TypeHole) 'True
type HoleAST p = AST p 'NoSugar HoleName      'Nothing
type HoleProgram = HoleAST 'TopLevel

-------------------- Instances -------------------------
instance IsList ConstraintSet where
    type Item ConstraintSet = Constraint
    fromList = coerce
    toList = coerce

---------------------- Show -----------------------------
instance Show ConstraintSet where
    show (ConstraintSet constraints) = intercalate "\n" (show <$> constraints)

instance Show Constraint where
    show (Equal t1 t2) = show t1 <> " =#= " <> show t2
    show (AcceptsArgs callee args) = "Accepts(" <> show callee <> ", (" <> intercalate ", " (show <$> args) <> "))"
    show (IsConstant c) = "IsConstant(" <> show c <> ")"
    show (IsVariable c) = "IsVariable(" <> show c <> ")"

instance Show TypeHole where
    show ( TypeHole        th          ) = "@" <> show th
    show ( TheType         t           ) = "#" <> show t
    show ( FuncHole        args rt     ) = "(" <> intercalate ", " (show <$> args) <> ") -> " <> show rt
    show ( ApplicationHole callee args ) = show callee <> "(" <> intercalate ", " (show <$> args) <> ")"

