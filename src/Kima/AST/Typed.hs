module Kima.AST.Typed where

import GHC.Generics hiding ((:+:))
import Data.Comp
import Control.Newtype.Generics
import Data.Kind
import Kima.AST.Parsed
import Kima.KimaTypes

-- | Annotate every signature in a sum with a constant type annotation
type family Annotate (f :: Type -> Type) (p :: Type) :: Type -> Type where
    Annotate (f :+: g) a = Annotate f a :+: Annotate g a
    Annotate f         a = f :&: a

type TypedStmtF e = Annotate (StmtF e) KType
type TypedStmtTerm e = Term (TypedStmtF e)
newtype TypedStmt = TypedStmt (TypedStmtTerm TypedExpr) deriving Generic

type TypedExprF e = Annotate (ExprF e) KType
type TypedExprTerm e = Term (TypedExprF e)
newtype TypedExpr = TypedExpr (TypedExprTerm TypedStmt) deriving Generic

instance Newtype TypedStmt
instance Newtype TypedExpr