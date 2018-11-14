module Kima.KimaTypes where

-- | Whether a KType is overloaded. Refer to the documentation of KType
data Overloaded = Overload | NoOverload

-- | The types of the kima programming language
-- | The ov type parameter defines whether functions are overloaded
-- | If yes, then functions can have multiple (possible) signatures.
data KType (ov :: Overloaded) where
  KFuncOverloaded :: [Signature (KType 'Overload)] -> KType 'Overload
  KString         :: KType o  
  KUnit           :: KType o
  KBool           :: KType o
  KInt            :: KType o
  KFloat          :: KType o
  KFunc           :: Signature (KType 'NoOverload) -> KType 'NoOverload

deriving instance Show (KType o)
deriving instance Eq (KType o)
deriving instance Ord (KType o)

data Signature kt = Signature {
  arguments :: [kt],
  returnType :: kt
} deriving (Eq, Ord)
($->) = Signature

instance Show kt => Show (Signature kt) where
  show Signature{arguments, returnType} = "(" ++ show arguments ++ ") -> " ++ show returnType

