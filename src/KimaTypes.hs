module KimaTypes where

data KType = KString | KUnit | KBool | KInt | KFloat | KFunc [Signature] 
  deriving (Eq, Show)

data Signature = Signature {
  arguments :: [KType],
  returnType :: KType
} deriving Eq
($->) = Signature

instance Show Signature where
  show Signature{arguments, returnType} = "(" ++ show arguments ++ ") -> " ++ show returnType

