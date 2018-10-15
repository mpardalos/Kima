module Typechecking.Types where

import           Data.Map.Lazy
import           AST

data TypedExpr = TypedExpr KType Expr
data TypedBlock = TypedBlock KType Block

data KType = KString | KUnit | KBool | KInt | KFloat | KFunc Signature 
  deriving (Eq, Show)

data Signature = Signature {
  arguments :: [KType],
  returnType :: KType
} deriving Eq
($->) = Signature


data TypeBinding = Constant { kType :: KType }
                 | Variable { kType :: KType }

data TypeCtx = TypeCtx {
  types :: Map Name KType,
  bindings :: Map Name TypeBinding
}

data TypeError = TypeMismatchError KType KType
               | ArgumentCountError Integer Integer
               | NotAFunctionError KType
               | LookupError Name
               | TypeLookupError Name
               | BinOpTypeError [(KType, KType)] (KType, KType)
               | UnaryOpTypeError [KType] KType


instance Show TypeError where
  show (TypeMismatchError expected actual) = "Expected " ++ show expected ++ ", got " ++ show actual
  show (NotAFunctionError  t) = "Expected a function but got " ++ show t
  show (LookupError     name) = "Name " ++ show name ++ " is not in scope"
  show (TypeLookupError name) = "Type name " ++ show name ++ " is not in scope"
  show (ArgumentCountError expected actual) = "Expected " ++ show expected ++ " arguments, received " ++ show actual
  show (BinOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual
  show (UnaryOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual

instance Show Signature where
  show Signature{arguments, returnType} = "(" ++ show arguments ++ ") -> " ++ show returnType

instance Show TypeCtx where show _ = "TypeCtx"

instance Semigroup TypeCtx where
  l <> r = TypeCtx (types l <> types r) (bindings l <> bindings r)

instance Monoid TypeCtx where
  mempty = TypeCtx empty empty
