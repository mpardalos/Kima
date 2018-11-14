module Kima.Typechecking.Monad where

import Control.Monad.Except
import Kima.Control.Monad.State.Extended
import Data.Map.Lazy

import Kima.AST.Common
import Kima.KimaTypes
import Kima.Typechecking.Types

newtype KTypeM a = KTypeM { runKTypeM :: StateT (TypeCtx (KType 'Overload)) (Either TypeError) a}
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState TypeCtx)

runTypeChecking :: TypeCtx -> KTypeM a -> Either TypeError a
runTypeChecking ctx = (`evalStateT` ctx) . runKTypeM

typeOk :: KTypeM ()
typeOk = return ()

lookupError :: Name -> KTypeM a
lookupError = throwError . LookupError

notAFunctionError :: KType -> KTypeM a
notAFunctionError = throwError . NotAFunctionError

typeMismatchError
  :: KType -- |Expected type
  -> KType -- |Actual type
  -> KTypeM a
typeMismatchError e a = throwError (TypeMismatchError e a)

getCtx :: KTypeM TypeCtx
getCtx = get

getBindings :: KTypeM (Map Name (Binding t))
getBindings = gets bindings

getTypes :: KTypeM (Map Name KType)
getTypes = gets types

bindName :: Binding t -> Name -> KTypeM ()
bindName b name = do
  ctx <- getCtx
  put (ctx { bindings = insert name b (bindings ctx) })

assert :: MonadError e m => Bool -> e -> m ()
assert False err = throwError err
assert True  _   = return ()

-- Assert that two types are equal, raise a TypeError if not
assertEqualTypes
  :: KType -- | Expected
  -> KType -- | Actual
  -> KTypeM ()
assertEqualTypes expected actual =
  assert (expected == actual) (TypeMismatchError expected actual)