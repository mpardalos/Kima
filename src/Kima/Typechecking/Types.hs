module Kima.Typechecking.Types where

import Data.Map.Lazy

import Kima.AST
import Kima.KimaTypes

type TypeBinding = Binding KTypeOv
data Binding t = Constant { kType :: t }
              |  Variable { kType :: t }
  deriving Show
                 
data TypeCtx = TypeCtx {
  types :: Map TypeName KTypeOv,
  bindings :: Map ParsedName (Binding KTypeOv)
}

data TypeError t = NoMatchingSignature ParsedName [t]
                 | TypeMismatchError t t
                 | NotAFunctionError t
                 | LookupError ParsedName
                 | TypeLookupError ParsedName
                 | NameAlreadyBoundError ParsedName
                 | BinOpTypeError [(t, t)] (t, t)
                 | UnaryOpTypeError [t] t
                 | ArgumentCountError Int Int

instance Show t => Show (TypeError t) where
  show (NoMatchingSignature funcName args) = "No implementation of " ++ show funcName ++ " accepts the types " ++ show args
  show (TypeMismatchError expected actual) = "Expected " ++ show expected ++ ", got " ++ show actual
  show (NotAFunctionError  t) = "Expected a function but got " ++ show t
  show (LookupError     name) = "Name " ++ show name ++ " is not in scope"
  show (TypeLookupError name) = "Type name " ++ show name ++ " is not in scope"
  show (BinOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual
  show (UnaryOpTypeError expected actual) = "Invalid types for operator. Expected one of " ++ show expected ++ " but received " ++ show actual
  show (NameAlreadyBoundError name) = "The variable " ++ show name ++ " is already bound."
  show (ArgumentCountError expected actual) = "Expected " ++ show expected ++ "arguments, but got " ++ show actual

instance Show TypeCtx where 
  show TypeCtx { types, bindings } = "TypeCtx " ++ show types ++ " | " ++ show bindings

instance Semigroup TypeCtx where
  l <> r = TypeCtx (types l <> types r) (bindings l <> bindings r)
