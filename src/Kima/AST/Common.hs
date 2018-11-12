module Kima.AST.Common where

import GHC.Records
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

type FuncDefRecord r name signature stmt = (HasField "name" r name, HasField "signature" r signature, HasField "body" r stmt)
data FuncDef name signature stmt = FuncDef {
    name :: name,
    signature :: signature,
    body :: stmt
}

type NamedSignatureRecord r name typeann = (HasField "arguments" r [(name, typeann)], HasField "returnType" r typeann)
data NamedSignature name typeann = NamedSignature {
    arguments :: [(name, typeann)],
    returnType :: typeann
} deriving (Eq, Functor, Foldable, Traversable)

instance Show EffectExpr where
    show (EffectName (Name str)) = "[" ++ str ++ "]"

instance Show TypeExpr where
    show (TypeName (Name str)) = "#" ++ str
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

instance (Show n, Show t) => Show (NamedSignature n t) where
    show NamedSignature {arguments, returnType} =
           show arguments
        ++ " -> " ++ show returnType

instance Show Name where
    show (Name str) = "{" ++ str ++ "}"

instance IsString Name where
    fromString = Name

instance {-# Overlappable #-} (Show n, NamedSignatureRecord sig n t, Show t, Show s) => Show (FuncDef n sig s) where
    show FuncDef { name, signature, body } =
        "fun " ++ show name 
        ++ " " ++ show (getField @"arguments" signature)  
        ++ " -> " ++ show (getField @"returnType" signature) 
        ++ " " ++ show body

instance Bifunctor (FuncDef n) where
    bimap f g FuncDef { name, signature, body } = FuncDef name (f signature) (g body)

instance Bifunctor NamedSignature where
    bimap f g NamedSignature { arguments, returnType } = NamedSignature (bimap f g <$> arguments) (g returnType)