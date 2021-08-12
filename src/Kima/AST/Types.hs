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
           -- | A user defined type is defined only by its name
           | KUserType String
  deriving (Eq, Ord, Generic)

-- | Resolved effects
data KEffect = KEffect (Maybe EffectName) [KOperation]
    deriving (Eq, Ord, Generic)

pattern PureEffect :: KEffect
pattern PureEffect = KEffect (Just "pure") []

pattern AnonymousEffect :: [KOperation] -> KEffect
pattern AnonymousEffect ops = KEffect Nothing ops

-- | A single effectful operation. Defined by its name and signature
data KOperation = KOperation String [KType] KType
    deriving (Eq, Ord, Generic)

-------- Resolved  types --------

instance Semigroup KEffect where
    PureEffect <> eff        = eff
    eff        <> PureEffect = eff
    (KEffect _ opsLeft) <> (KEffect _ opsRight) =
        KEffect Nothing (opsLeft <> opsRight)

instance Monoid KEffect where
    mempty = PureEffect

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
        pretty name
        <> encloseSep lparen rparen comma (pretty <$> args)
        <+> "->"
        <+> pretty rt

deriving instance Show KEffect

instance Pretty KEffect where
    pretty (KEffect (Just name) _ops) = pretty name
    pretty (KEffect Nothing ops) = encloseSep lbrace rbrace comma (pretty <$> ops)

instance Pretty KType where
    pretty KString = "String"
    pretty KUnit   = "Unit"
    pretty KBool   = "Bool"
    pretty KInt    = "Int"
    pretty KFloat  = "Float"
    pretty (KFunc arguments effect returnType) =
        encloseSep lparen rparen comma (pretty <$> arguments)
            <+> ":"
            <+> pretty effect
            <+> "->"
            <+> pretty returnType
    pretty (KUserType n) = pretty n
