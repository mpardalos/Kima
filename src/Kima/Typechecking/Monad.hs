module Kima.Typechecking.Monad where

import Control.Monad.Except
import Kima.Control.Monad.State.Extended
import Data.Map.Lazy

import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.Types

newtype KTypeM a = KTypeM { 
    runKTypeM :: StateT TypeCtx 
                (Either (TypeError KTypeOv)) 
                a
} deriving (Functor, Applicative, Monad, 
            MonadError (TypeError KTypeOv), 
            MonadState TypeCtx)

runTypeChecking :: TypeCtx -> KTypeM a -> Either (TypeError KTypeOv) a
runTypeChecking ctx = (`evalStateT` ctx) . runKTypeM

typeOk :: KTypeM ()
typeOk = return ()

lookupError :: ParsedName -> KTypeM a
lookupError = throwError . LookupError

notAFunctionError :: KTypeOv -> KTypeM a
notAFunctionError = throwError . NotAFunctionError

typeMismatchError
  :: KTypeOv -- |Expected type
  -> KTypeOv -- |Actual type
  -> KTypeM a
typeMismatchError e a = throwError (TypeMismatchError e a)

getCtx :: KTypeM TypeCtx
getCtx = get

getBindings :: KTypeM (Map ParsedName TypeBinding)
getBindings = gets bindings

getTypes :: KTypeM (Map TypeName KTypeOv)
getTypes = gets types

bindName :: TypeBinding -> ParsedName -> KTypeM ()
bindName b name = do
  ctx <- getCtx
  put (ctx { bindings = insert name b (bindings ctx) })

assert :: MonadError e m => Bool -> e -> m ()
assert False err = throwError err
assert True  _   = return ()

-- Assert that two types are equal, raise a TypeError if not
assertEqualTypes
  :: KTypeOv -- | Expected
  -> KTypeOv -- | Actual
  -> KTypeM ()
assertEqualTypes expected actual =
  assert (expected == actual) (TypeMismatchError expected actual)