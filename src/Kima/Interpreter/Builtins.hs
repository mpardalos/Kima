module Kima.Interpreter.Builtins where

import Control.Monad.IO.Class
import Kima.Interpreter.Types
import Kima.AST.Desugared
import Data.Map

baseEnv :: Environment Value
baseEnv = Environment $ fromList
    [ (Builtin AddOp, BuiltinFunction2 $ liftNumOp (+))
    , (Builtin SubOp, BuiltinFunction2 $ liftNumOp (-))
    , (Builtin MulOp, BuiltinFunction2 $ liftNumOp (*))
    , (Builtin ModOp, BuiltinFunction2 $ liftIntegralOp mod)
    , (Builtin DivOp, BuiltinFunction2 $ kimaDivision)
    , (Builtin PrintFunc, BuiltinFunction1 kimaPrint)
    ]

kimaPrint :: (MonadRE m, MonadIO m) => Value -> m Value
kimaPrint (String str) = do 
    liftIO (putStrLn str)
    return Unit
kimaPrint _ = runtimeError

kimaDivision :: (MonadRE m) => Value -> Value -> m Value
kimaDivision (Integer l) (Integer r) = return $ Integer (l `div` r)
kimaDivision (Integer l) (Float r) = return $ Float (fromInteger l / r)
kimaDivision (Float l) (Integer r) = return $ Float (l / fromInteger r)
kimaDivision (Float l) (Float r) = return $ Float (l / r)
kimaDivision _ _ = runtimeError

liftIntegralOp :: (MonadRE m) => (forall a. Integral a => a -> a -> a) -> Value -> Value -> m Value
liftIntegralOp op (Integer l) (Integer r) = return $ Integer (l `op` r)
liftIntegralOp _ _ _ = runtimeError

liftNumOp :: (MonadRE m) => (forall a. Num a => a -> a -> a) -> Value -> Value -> m Value
liftNumOp op (Integer l) (Integer r) = return $ Integer (l `op` r)
liftNumOp op (Integer l) (Float r) = return $ Float (fromInteger l `op` r)
liftNumOp op (Float l) (Integer r) = return $ Float (l `op` fromInteger r)
liftNumOp op (Float l) (Float r) = return $ Float (l `op` r)
liftNumOp _ _ _ = runtimeError