-- | Types in the AST

module Kima.AST.Types where

import           Data.String
import           Data.Text.Prettyprint.Doc

type TypeName = String

data TypeExpr
    -- | Just a single type
    = TypeName TypeName
    -- | Function signature
    | SignatureType [TypeExpr] TypeExpr
    deriving Eq

instance Show TypeExpr where
    show (TypeName s           ) = "#\"" ++ s ++ "\""
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

instance Pretty TypeExpr where
    pretty (TypeName name) = pretty name
    pretty (SignatureType args returnType) =
        tupled (pretty <$> args) <+> "->" <+> pretty returnType

instance IsString TypeExpr where
    fromString = TypeName
