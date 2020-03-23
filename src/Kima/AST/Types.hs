{-# LANGUAGE OverloadedStrings #-}
module Kima.AST.Types where

import Data.String
import Data.Text.Prettyprint.Doc
import GHC.Generics
import GHC.Exts

-------- Parsed types --------
type EffectName = String
newtype ParsedEffect = EffectNames [EffectName]
    deriving (Show, Eq, Ord, IsList, Semigroup, Monoid)

type TypeName = String

data ParsedTypeExpr
    -- | Just a single type
    = ParsedTypeName TypeName
    -- | Function signature
    | ParsedSignatureType [ParsedTypeExpr] (Maybe ParsedEffect) ParsedTypeExpr
    deriving Eq

data TypeExpr
    -- | Just a single type
    = TypeName TypeName
    -- | Function signature
    | SignatureType [TypeExpr] ParsedEffect TypeExpr
    deriving Eq

-------- Resolved  types --------

-- | The types of the kima programming language
data KType = KString | KUnit | KBool | KInt | KFloat
           | KFunc [KType] KEffect KType
           -- | A user defined type is defined by its name and fields
           | KUserType String [(String, KType)]
  deriving (Eq, Ord, Generic)

-- | Resolved effects
type KEffect = [KOperation]

-- | A single effectful operation. Defined by its name and signature
data KOperation = KOperation String [KType] KType
    deriving (Eq, Ord, Generic)

-------- Resolved  types --------

instance IsString ParsedEffect where
    fromString s = EffectNames [s]

instance Pretty ParsedEffect where
    pretty (EffectNames [eff]) = pretty eff
    pretty (EffectNames effects) =
        encloseSep lbrace rbrace comma (pretty <$> effects)

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

instance Show KOperation where
    show = show . pretty

instance Pretty KOperation where
    pretty (KOperation name args rt) =
        "effect"
        <+> pretty name
        <> encloseSep lparen rparen comma (pretty <$> args)
        <+> "->"
        <+> pretty rt

instance Pretty KType where
  pretty KString          = "String"
  pretty KUnit            = "Unit"
  pretty KBool            = "Bool"
  pretty KInt             = "Int"
  pretty KFloat           = "Float"
  pretty (KFunc arguments effect returnType) =
      encloseSep lparen rparen comma (pretty <$> arguments)
          <+> ":"
          <+> prettyEffect
          <+> "->"
          <+> pretty returnType
      where
          prettyEffect = case effect of
              [] -> "pure"
              _ -> pretty effect
  pretty (KUserType n _f) = pretty n
