module Kima.KimaTypes where

data KType = KString | KUnit | KBool | KInt | KFloat | KFunc [Signature] 
  deriving (Show, Eq, Ord)

data Signature = Signature {
  arguments :: [KType],
  returnType :: KType
} deriving (Eq, Ord)
($->) = Signature

instance Show Signature where
  show Signature{arguments, returnType} = "(" ++ show arguments ++ ") -> " ++ show returnType

