module Kima.AST.Expression where

data Binary e  = Add e e 
               | Sub e e 
               | Div e e 
               | Mul e e 
               | Mod e e
    deriving (Show, Functor, Foldable, Traversable)
data Unary e   = Negate e 
               | Invert e
    deriving (Show, Functor, Foldable, Traversable)
data Literal   = IntExpr Integer
               | FloatExpr Double
               | BoolExpr Bool
               | StringExpr String 
    deriving Show
