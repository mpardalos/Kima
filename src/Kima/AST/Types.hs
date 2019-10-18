module Kima.AST.Types where

import Data.List
import Data.String
import Data.Text.Prettyprint.Doc
import GHC.Generics

-- | The types of the kima programming language
data KType = KString | KUnit | KBool | KInt | KFloat
           | KFunc (Signature KType)
           -- | A user defined type is defined by its name and fields
           | KUserType String [(String, KType)]
  deriving (Eq, Ord, Generic)

data Signature kt = Signature {
  arguments :: [kt],
  returnType :: kt
} deriving (Eq, Ord, Generic)
($->) = Signature

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

-- | Useful because typeexprs are optional.
instance IsString (Maybe TypeExpr) where
    fromString = Just . TypeName


instance Show t => Show (Signature t) where
  show Signature{arguments, returnType} = "(" ++ intercalate ", " (show <$> arguments) ++ ") -> " ++ show returnType

instance Show KType where
  show = show . pretty

instance Pretty KType where
  pretty KString          = "String"
  pretty KUnit            = "Unit"
  pretty KBool            = "Bool"
  pretty KInt             = "Int"
  pretty KFloat           = "Float"
  pretty (KFunc sig)      = "[" <> viaShow sig <> "]"
  pretty (KUserType n _f) = pretty n
