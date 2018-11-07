module AST.Expression where

import AST.Common

import Data.Bifunctor

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Desugar

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
data FuncExpr s e    = FuncExpr NamedSignature s
data Call e          = CallExpr e [e]

deriving instance Functor BinExpr
deriving instance Functor UnaryExpr
deriving instance Functor Literal
deriving instance Functor Identifier
deriving instance Functor (FuncExpr s)
deriving instance Functor Call

instance Bifunctor FuncExpr where
    bimap f _ (FuncExpr sig body) = FuncExpr sig (f body)

$(derive [makeShowF, makeFoldable, makeTraversable, smartAConstructors, smartConstructors]
 [''BinExpr, ''UnaryExpr, ''Literal, ''Identifier, ''FuncExpr, ''Call])

instance (Functor g, Call :<: g, Identifier :<: g) => Desugar BinExpr g where
    desugHom (Add l r) = iCallExpr (iIdentifierExpr "b__add") [Hole l, Hole r] 
    desugHom (Sub l r) = iCallExpr (iIdentifierExpr "b__sub") [Hole l, Hole r] 
    desugHom (Div l r) = iCallExpr (iIdentifierExpr "b__div") [Hole l, Hole r] 
    desugHom (Mul l r) = iCallExpr (iIdentifierExpr "b__mul") [Hole l, Hole r] 
    desugHom (Mod l r) = iCallExpr (iIdentifierExpr "b__mod") [Hole l, Hole r] 

instance (Functor g, Call :<: g, Identifier :<: g) => Desugar UnaryExpr g where
    desugHom (Negate e) = iCallExpr (iIdentifierExpr "b__add") [Hole e]
    desugHom (Invert e) = iCallExpr (iIdentifierExpr "b__sub") [Hole e]