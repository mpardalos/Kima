module Kima.AST.Common where

import Data.Bifunctor
import Data.String

newtype Name = Name String
    deriving (Eq, Ord)

-- Effects
data EffectExpr = EffectName Name

-- Types
data TypeExpr = TypeName Name
              | SignatureType [TypeExpr] TypeExpr
    deriving Eq

data FuncDef t s = FuncDef {
    name :: Name,
    signature :: NamedSignature t,
    body :: s
}

data NamedSignature t = NamedSignature {
    arguments :: [(Name, t)],
    returnType :: t
} deriving (Eq, Functor, Foldable, Traversable)

instance Show EffectExpr where
    show (EffectName (Name str)) = "[" ++ str ++ "]"

instance Show TypeExpr where
    show (TypeName (Name str)) = "#" ++ str
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

instance Show t => Show (NamedSignature t) where
    show NamedSignature {arguments, returnType} =
           show arguments
        ++ " -> " ++ show returnType

instance Show Name where
    show (Name str) = "{" ++ str ++ "}"

instance IsString Name where
    fromString = Name

instance (Show t, Show s) => Show (FuncDef t s) where
    show FuncDef { name, signature, body } =
        "fun " ++ show name 
        ++ " " ++ show (arguments signature)  
        ++ " -> " ++ show (returnType signature) 
        ++ " " ++ show body

instance Bifunctor FuncDef where
    bimap f g FuncDef { name, signature, body } = FuncDef name (f <$> signature) (g body)