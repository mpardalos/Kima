module Kima.Typechecking.Bidirectional where

import Control.Monad.State.Extended
import Control.Monad.Except

import qualified Data.Map as Map
import qualified Data.Set as Set

import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.TypeCtx

type MonadTC m = (MonadState TypeCtx m, MonadError String m)

infer :: MonadTC m => AST 'Expr TypeAnnotated -> m KType
infer (LiteralE (IntExpr _)) = pure KInt
infer (LiteralE (FloatExpr _)) = pure KFloat
infer (LiteralE (BoolExpr _)) = pure KBool
infer (LiteralE (StringExpr _)) = pure KString
infer (IdentifierE name) = Map.lookup name <$> gets bindings >>= \case
    Just _binding -> _inferIdentifierType
    Nothing -> throwError (show name ++ " is not present in env")
infer (FuncExpr args rt body) = do
    let expectedType = KFunc ((snd <$> args) $-> rt)
    withState (addArgs args) $
        checkReturns expectedType body
    return expectedType
infer (Call callee args) = do
    calleeType <- infer callee
    case calleeType of
        (KFunc (Signature argTypes rt)) -> do
            assert (length argTypes == length args) "Unequal number of args"
            zipWithM_ check argTypes args
            return rt
        _ -> throwError "Calling a value of non-function type"
