module Kima.Typechecking.Types where

import Data.Map
import Kima.AST
import Kima.KimaTypes

data Constraint = Equal TypeHole TypeHole
                | IsConstant TypeHole
                | IsVariable TypeHole
    deriving (Show, Eq, Ord)
(=$=) = Equal

-- Types we have to solve for
data TypeHole = TypeHole Int
              | TheType KType
              | FuncHole [TypeHole] TypeHole
              | ApplicationHole TypeHole [TypeHole]
    deriving (Show, Eq, Ord)

type HoleMap          = Map DesugaredName TypeHole
type HoleSubstitution = Map TypeHole      KType

type HoleName  = GenericName ('Just TypeHole) 'True
type HoleAST p = AST p 'NoSugar HoleName      'Nothing
type HoleProgram = HoleAST 'TopLevel