{-# LANGUAGE OverloadedStrings #-}
module Kima.AST.Types where

import Kima.AST.Effects

import Data.String
import Data.Text.Prettyprint.Doc
import GHC.Generics

-- | The types of the kima programming language
data KType = KString | KUnit | KBool | KInt | KFloat
           | KFunc [KType] Effect KType
           -- | A user defined type is defined by its name and fields
           | KUserType String [(String, KType)]
  deriving (Eq, Ord, Generic)

type TypeName = String

data ParsedTypeExpr
    -- | Just a single type
    = ParsedTypeName TypeName
    -- | Function signature
    | ParsedSignatureType [ParsedTypeExpr] (Maybe Effect) ParsedTypeExpr
    deriving Eq

data TypeExpr
    -- | Just a single type
    = TypeName TypeName
    -- | Function signature
    | SignatureType [TypeExpr] Effect TypeExpr
    deriving Eq

instance Show ParsedTypeExpr where
    show (ParsedTypeName s           ) = "#\"" ++ s ++ "\""
    show (ParsedSignatureType args eff rt) = "#( (" ++ show args ++ ") -> " ++ show eff ++ show rt ++ ")"

instance Pretty ParsedTypeExpr where
    pretty (ParsedTypeName name) = pretty name
    pretty (ParsedSignatureType args (Just eff) returnType) =
        tupled (pretty <$> args) <+> "=>" <+> pretty eff <+> "->" <+> pretty returnType
    pretty (ParsedSignatureType args Nothing returnType) =
        tupled (pretty <$> args) <+> "->" <+> pretty returnType

instance Show TypeExpr where
    show (TypeName s           ) = "#\"" ++ s ++ "\""
    show (SignatureType args eff rt) = "#( (" ++ show args ++ ") -> " ++ show eff ++ show rt ++ ")"

instance Pretty TypeExpr where
    pretty (TypeName name) = pretty name
    pretty (SignatureType args eff returnType) =
        tupled (pretty <$> args) <+> "->" <+> pretty eff <+> pretty returnType

instance IsString ParsedTypeExpr where
    fromString = ParsedTypeName

instance IsString TypeExpr where
    fromString = TypeName

-- | Useful because typeexprs are optional.
instance IsString (Maybe ParsedTypeExpr) where
    fromString = Just . ParsedTypeName

instance IsString (Maybe TypeExpr) where
    fromString = Just . TypeName

-- instance Show t => Show (Signature t) where
--   show Signature{arguments, returnType} = "(" ++ intercalate ", " (show <$> arguments) ++ ") -> " ++ show returnType

instance Show KType where
  show = show . pretty

instance Pretty KType where
  pretty KString          = "String"
  pretty KUnit            = "Unit"
  pretty KBool            = "Bool"
  pretty KInt             = "Int"
  pretty KFloat           = "Float"
  pretty (KFunc arguments effect returnType) =
      encloseSep lparen rparen comma (pretty <$> arguments)
          <+> "->"
          <+> pretty effect
          <+> pretty returnType
  pretty (KUserType n _f) = pretty n
