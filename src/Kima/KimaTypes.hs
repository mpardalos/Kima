module Kima.KimaTypes where

import Data.List

-- | The types of the kima programming language
data KType = KString | KUnit | KBool | KInt | KFloat | KFunc (Signature KType)
  deriving (Eq, Ord)

data Signature kt = Signature {
  arguments :: [kt],
  returnType :: kt
} deriving (Eq, Ord)
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
