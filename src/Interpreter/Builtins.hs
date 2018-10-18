module Interpreter.Builtins where

import Interpreter.Types
import AST

evalBinOp :: (MonadEnv s m, MonadRE e m) => BinOp -> Value -> Value -> m Value
evalBinOp Add (Integer l) (Integer r) = return $ Integer (l + r)
evalBinOp Add (Integer l) (Float r) = return . Float $ fromInteger l + r
evalBinOp Add (Float l) (Integer r) = return . Float $ l + fromInteger r
evalBinOp Add (Float l) (Float r) = return . Float $ l + r
evalBinOp Add _ _ = runtimeError
evalBinOp Sub (Integer l) (Integer r) = return $ Integer (l - r)
evalBinOp Sub (Integer l) (Float r) = return . Float $ fromInteger l - r
evalBinOp Sub (Float l) (Integer r) = return . Float $ l - fromInteger r
evalBinOp Sub (Float l) (Float r) = return . Float $ l - r
evalBinOp Sub _ _ = runtimeError
evalBinOp Div (Integer l) (Integer r) = return $ Integer (l `quot` r)
evalBinOp Div (Integer l) (Float r) = return . Float $ fromInteger l / r
evalBinOp Div (Float l) (Integer r) = return . Float $ l / fromInteger r
evalBinOp Div (Float l) (Float r) = return . Float $ l / r
evalBinOp Div _ _ = runtimeError
evalBinOp Mul (Integer l) (Integer r) = return $ Integer (l * r)
evalBinOp Mul (Integer l) (Float r) = return . Float $ fromInteger l * r
evalBinOp Mul (Float l) (Integer r) = return . Float $ l * fromInteger r
evalBinOp Mul (Float l) (Float r) = return . Float $ l * r
evalBinOp Mul _ _ = runtimeError
evalBinOp Mod (Integer l) (Integer r) = return $ Integer (l `mod` r)
evalBinOp Mod _ _ = runtimeError

evalUnaryOp :: (MonadEnv s m, MonadRE e m) => UnaryOp -> Value -> m Value
evalUnaryOp Negate (Integer val) = return $ Integer (-val)
evalUnaryOp Negate (Float val) = return $ Float (-val)
evalUnaryOp Negate _ = runtimeError
evalUnaryOp Invert (Bool val) = return $ Bool (not val)
evalUnaryOp Invert _ = runtimeError