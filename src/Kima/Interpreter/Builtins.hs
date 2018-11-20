module Kima.Interpreter.Builtins where

import Control.Monad.IO.Class
import Kima.Interpreter.Types
import Kima.AST
import Kima.KimaTypes
import Data.Map

baseEnv :: Environment Value
baseEnv = Environment $ fromList
    [ (Builtin AddOp (KFunc ([KInt, KInt] $-> KInt)), BuiltinFunction2 $ liftNumOp (+))
    , (Builtin AddOp (KFunc ([KInt, KFloat] $-> KInt)), BuiltinFunction2 $ liftNumOp (+))
    , (Builtin AddOp (KFunc ([KFloat, KInt] $-> KFloat)), BuiltinFunction2 $ liftNumOp (+))
    , (Builtin AddOp (KFunc ([KFloat, KFloat] $-> KInt)), BuiltinFunction2 $ liftNumOp (+))

    , (Builtin SubOp (KFunc ([KInt, KInt] $-> KInt)), BuiltinFunction2 $ liftNumOp (-))
    , (Builtin SubOp (KFunc ([KInt, KFloat] $-> KInt)), BuiltinFunction2 $ liftNumOp (-))
    , (Builtin SubOp (KFunc ([KFloat, KInt] $-> KFloat)), BuiltinFunction2 $ liftNumOp (-))
    , (Builtin SubOp (KFunc ([KFloat, KFloat] $-> KInt)), BuiltinFunction2 $ liftNumOp (-))

    , (Builtin MulOp (KFunc ([KInt, KInt] $-> KInt)), BuiltinFunction2 $ liftNumOp (*))
    , (Builtin MulOp (KFunc ([KInt, KFloat] $-> KInt)), BuiltinFunction2 $ liftNumOp (*))
    , (Builtin MulOp (KFunc ([KFloat, KInt] $-> KFloat)), BuiltinFunction2 $ liftNumOp (*))
    , (Builtin MulOp (KFunc ([KFloat, KFloat] $-> KInt)), BuiltinFunction2 $ liftNumOp (*))

    , (Builtin DivOp (KFunc ([KInt, KInt] $-> KInt)), BuiltinFunction2 $ kimaDivision)
    , (Builtin DivOp (KFunc ([KInt, KFloat] $-> KInt)), BuiltinFunction2 $ kimaDivision)
    , (Builtin DivOp (KFunc ([KFloat, KInt] $-> KFloat)), BuiltinFunction2 $ kimaDivision)
    , (Builtin DivOp (KFunc ([KFloat, KFloat] $-> KInt)), BuiltinFunction2 $ kimaDivision)

    , (Builtin ModOp (KFunc ([KInt, KInt] $-> KInt)), BuiltinFunction2 $ liftIntegralOp mod)

    , (Builtin PrintFunc (KFunc ([KString] $-> KString)), BuiltinFunction1 kimaPrint)
    , (Builtin PrintFunc (KFunc ([KInt] $-> KString)), BuiltinFunction1 kimaPrint)
    , (Builtin PrintFunc (KFunc ([KFloat] $-> KString)), BuiltinFunction1 kimaPrint)
    , (Builtin PrintFunc (KFunc ([KBool] $-> KString)), BuiltinFunction1 kimaPrint)
    , (Builtin PrintFunc (KFunc ([KUnit] $-> KString)), BuiltinFunction1 kimaPrint)
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