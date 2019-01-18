module Kima.KimaTypes where

import Data.List
import Data.Text.Prettyprint.Doc
import GHC.Generics

-- | The types of the kima programming language
data KType = KString | KUnit | KBool | KInt | KFloat | KFunc (Signature KType)
  deriving (Eq, Ord, Generic)

data Signature kt = Signature {
  arguments :: [kt],
  returnType :: kt
} deriving (Eq, Ord, Generic)
($->) = Signature

instance Show t => Show (Signature t) where
  show Signature{arguments, returnType} = "(" ++ intercalate ", " (show <$> arguments) ++ ") -> " ++ show returnType

instance Show KType where
  show KString = "KString"
  show KUnit = "KUnit"
  show KBool = "KBool"
  show KInt = "KInt"
  show KFloat = "KFloat"
  show (KFunc sig) = "[" <> show sig <> "]"

instance Pretty KType where
  pretty KString     = "String"
  pretty KUnit       = "Unit"
  pretty KBool       = "Bool"
  pretty KInt        = "Int"
  pretty KFloat      = "Float"
  pretty (KFunc sig) = "[" <> viaShow sig <> "]"