module Kima.Interpreter.Builtins where

import Control.Monad.IO.Class
import Kima.Interpreter.Types
import Data.Map

baseEnv :: Environment Value
baseEnv = Environment $ fromList
    [ ("b__add", BuiltinFunction2 $ liftNumOp (+))
    , ("b__sub", BuiltinFunction2 $ liftNumOp (-))
    , ("b__mul", BuiltinFunction2 $ liftNumOp (*))
    , ("b__mod", BuiltinFunction2 $ liftIntegralOp mod)
    , ("b__div", BuiltinFunction2 $ kimaDivision)
    , ("print", BuiltinFunction1 kimaPrint)
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