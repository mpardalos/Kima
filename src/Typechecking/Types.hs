module Typechecking.Types where

import Data.Map.Strict
import Control.Monad.State.Extended
import Control.Monad.Except
import AST

data TypedExpr = TypedExpr KType Expr
data TypedBlock = TypedBlock KType Block

data Signature = Signature {
  arguments :: [KType],
  returnType :: KType
} deriving Eq
($->) = Signature

data KType = KString | KUnit | KBool | KInt | KFloat | KFunc Signature
  deriving (Eq, Show)

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

newtype KTypeM a = KTypeM { runKTypeM :: StateT TypeCtx (Either TypeError) a}
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState TypeCtx)

instance Show Signature where
  show Signature{arguments, returnType} = "(" ++ show arguments ++ ") -> " ++ show returnType

instance Show TypeCtx where show _ = "TypeCtx"

instance Semigroup TypeCtx where
  l <> r = TypeCtx (types l <> types r) (bindings l <> bindings r)

instance Monoid TypeCtx where
  mempty = TypeCtx empty empty

typeOk :: KTypeM ()
typeOk = return ()

lookupError :: Name -> KTypeM a
lookupError = throwError . LookupError

notAFunctionError :: KType -> KTypeM a
notAFunctionError = throwError . NotAFunctionError

typeMismatchError :: KType -- |Expected type
          -> KType -- |Actual type
          -> KTypeM a
typeMismatchError e a = throwError (TypeMismatchError e a)

getCtx :: KTypeM TypeCtx
getCtx = get

getBindings :: KTypeM (Map Name TypeBinding)
getBindings = gets bindings

getTypes :: KTypeM (Map Name KType)
getTypes = gets types

bindName :: TypeBinding -> Name -> KTypeM ()
bindName b name = do
  ctx <- getCtx
  put (ctx {bindings = insert name b (bindings ctx)})

assert :: MonadError e m => Bool -> e -> m ()
assert False err = throwError err
assert True  _   = return ()

-- Assert that two types are equal, raise a TypeError if not
assertEqualTypes :: KType -- | Expected
                 -> KType -- | Actual
                 -> KTypeM ()
assertEqualTypes expected actual = assert (expected == actual) (TypeMismatchError expected actual)
