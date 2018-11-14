module Kima.Typechecking.Types where

import Data.Map.Lazy

import Kima.AST.Common
import Kima.KimaTypes

data Binding t = Constant { kType :: t }
               | Variable { kType :: t }
  deriving Show
                 
data TypeCtx t = TypeCtx {
  types :: Map Name t,
  bindings :: Map Name (Binding t)
}

data TypeError t = NoMatchingSignature Name [t]
                 | TypeMismatchError t t
                 | NotAFunctionError t
                 | LookupError Name
                 | TypeLookupError Name
                 | NameAlreadyBoundError Name
                 | BinOpTypeError [(t, t)] (t, t)
                 | UnaryOpTypeError [t] t

instance Show t => Show (TypeError t) where
  show (NoMatchingSignature funcName args) = "No implementation of " ++ show funcName ++ " accepts the types " ++ show args
  show (TypeMismatchError expected actual) = "Expected " ++ show expected ++ ", got " ++ show actual
  show (NotAFunctionError  t) = "Expected a function but got " ++ show t
  show (LookupError     name) = "Name " ++ show name ++ " is not in scope"
  show (TypeLookupError name) = "Type name " ++ show name ++ " is not in scope"
  show (BinOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual
  show (UnaryOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual
  show (NameAlreadyBoundError name) = "The variable " ++ show name ++ " is already bound."

instance Show t => Show (TypeCtx t) where 
  show TypeCtx { types, bindings } = "TypeCtx " ++ show types ++ " | " ++ show bindings

instance Semigroup (TypeCtx t) where
  l <> r = TypeCtx (types l <> types r) (bindings l <> bindings r)
