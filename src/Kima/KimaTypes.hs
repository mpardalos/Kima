module Kima.KimaTypes where

-- | The types of the kima programming language
data KType = KString | KUnit | KBool | KInt | KFloat | KFunc (Signature KType)
  deriving (Show, Eq, Ord)

data Signature kt = Signature {
  arguments :: [kt],
  returnType :: kt
} deriving (Eq, Ord)
($->) = Signature

instance Show t => Show (Signature t) where
  show Signature{arguments, returnType} = "(" ++ show arguments ++ ") -> " ++ show returnType

