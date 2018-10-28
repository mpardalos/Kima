module AST.Expression where

import Data.Comp.Derive
import Data.Comp.Sum

import AST.Common

-- Expressions
type ExprF s = (BinExpr :+: UnaryExpr :+: Literal :+: Identifier :+: FuncExpr s :+: Call)
type DesugaredExprF s = (Literal :+: Identifier :+: FuncExpr s :+: Call)

data BinExpr e       = Add e e 
                     | Sub e e 
                     | Div e e 
                     | Mul e e 
                     | Mod e e
data UnaryExpr e     = Negate e 
                     | Invert e
data Literal e       = IntExpr Integer
                     | FloatExpr Double
                     | BoolExpr Bool
                     | StringExpr String 
newtype Identifier e = IdentifierExpr Name
data FuncExpr s e    = FuncExpr NamedSignature (Block s)
data Call e          = CallExpr e [e]

deriving instance Functor BinExpr
deriving instance Functor UnaryExpr
deriving instance Functor Literal
deriving instance Functor Identifier
deriving instance Functor (FuncExpr s)
deriving instance Functor Call

$(derive [makeShowF, makeFoldable, makeTraversable, smartAConstructors, smartConstructors]
 [''BinExpr, ''UnaryExpr, ''Literal, ''Identifier, ''FuncExpr, ''Call])