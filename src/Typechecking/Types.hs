module Typechecking.Types where

import AST
import Data.Map.Lazy

data TypedExpr = TypedExpr KType Expr
data TypedBlock = TypedBlock KType Stmt

data KType = KString | KUnit | KBool | KInt | KFloat | KFunc [Signature] 
  deriving (Eq, Show)

data Signature = Signature {
  arguments :: [KType],
  returnType :: KType
} deriving Eq
($->) = Signature


data TypeBinding = Constant { kType :: KType }
                 | Variable { kType :: KType }
  deriving Show
                 
data TypeCtx = TypeCtx {
  types :: Map Name KType,
  bindings :: Map Name TypeBinding
}

data TypeError = NoMatchingSignature Name [KType]
               | TypeMismatchError KType KType
               | NotAFunctionError KType
               | LookupError Name
               | TypeLookupError Name
               | BinOpTypeError [(KType, KType)] (KType, KType)
               | UnaryOpTypeError [KType] KType

instance Show TypeError where
  show (NoMatchingSignature funcName args) = "No implementation of " ++ show funcName ++ " accepts the types " ++ show args
  show (TypeMismatchError expected actual) = "Expected " ++ show expected ++ ", got " ++ show actual
  show (NotAFunctionError  t) = "Expected a function but got " ++ show t
  show (LookupError     name) = "Name " ++ show name ++ " is not in scope"
  show (TypeLookupError name) = "Type name " ++ show name ++ " is not in scope"
  show (BinOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual
  show (UnaryOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual

instance Show Signature where
  show Signature{arguments, returnType} = "(" ++ show arguments ++ ") -> " ++ show returnType

instance Show TypeCtx where 
  show TypeCtx { types, bindings } = "TypeCtx " ++ show types ++ " | " ++ show bindings

instance Semigroup TypeCtx where
  l <> r = TypeCtx (types l <> types r) (bindings l <> bindings r)
