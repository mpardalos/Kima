module AST.Common where

import Data.String

newtype Name = Name String
    deriving (Eq, Ord)

newtype Block s = Block [s] deriving (Functor, Foldable, Traversable)

newtype Program s = Program [FuncDef s]

data FuncDef s = FuncDef {
    name :: Name,
    signature :: NamedSignature,
    body :: Block s
}

-- Effects
data EffectExpr = EffectName Name

-- Types
data TypeExpr = TypeName Name
              | SignatureType [TypeExpr] TypeExpr
    deriving Eq

data NamedSignature = NamedSignature {
         arguments :: [(Name, TypeExpr)],
         returnType :: TypeExpr
     } deriving Eq

instance Show EffectExpr where
    show (EffectName (Name str)) = "[" ++ str ++ "]"

instance Show TypeExpr where
    show (TypeName (Name str)) = "#" ++ str
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

instance Show NamedSignature where
    show NamedSignature {arguments, returnType} =
           show arguments
        ++ " -> " ++ show returnType

instance Show Name where
    show (Name str) = "{" ++ str ++ "}"

instance IsString Name where
    fromString = Name

instance Show s => Show (Block s) where
    show (Block blk) = "{\n" ++ blockShow blk ++ "}"
        where
            blockShow :: [s] -> String
            blockShow [] = ""
            blockShow (stmt : stmts) = "\t" ++ show stmt ++ "\n" ++ blockShow stmts

instance Show s => Show (FuncDef s) where
    show FuncDef { name, signature=sig, body } =
        "fun " ++ show name ++ " " ++ show (arguments sig)  ++ " -> " ++ show (returnType sig) ++ " " ++ show body