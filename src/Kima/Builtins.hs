{-# LANGUAGE OverloadedLists #-}
module Kima.Builtins where

import Control.Monad.Except
import Data.Functor
import qualified Data.Map as Map

import Kima.Interpreter.Types
import Kima.Typechecking.Types
import Kima.AST
import Kima.KimaTypes

baseTypeCtx :: TypeCtx
baseTypeCtx = Map.foldlWithKey combine Map.empty (unEnv baseEnv)
  where
    combine typeCtx name _ =
        Map.insertWith (<>) (deTypeAnnotate name) [nameType name] typeCtx

baseEnv :: Environment Value
baseEnv = Environment
    [ ( TBuiltin AddOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction2 $ liftNumOp (+))
    , ( TBuiltin AddOp     (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction2 $ liftNumOp (+))
    , ( TBuiltin AddOp     (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction2 $ liftNumOp (+))
    , ( TBuiltin AddOp     (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction2 $ liftNumOp (+))
    , ( TBuiltin SubOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction2 $ liftNumOp (-))
    , ( TBuiltin SubOp     (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction2 $ liftNumOp (-))
    , ( TBuiltin SubOp     (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction2 $ liftNumOp (-))
    , ( TBuiltin SubOp     (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction2 $ liftNumOp (-))
    , ( TBuiltin MulOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction2 $ liftNumOp (*))
    , ( TBuiltin MulOp     (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction2 $ liftNumOp (*))
    , ( TBuiltin MulOp     (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction2 $ liftNumOp (*))
    , ( TBuiltin MulOp     (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction2 $ liftNumOp (*))
    , ( TBuiltin GTOp      (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction2 $ liftComparison (>))
    , ( TBuiltin GTOp      (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction2 $ liftComparison (>))
    , ( TBuiltin GTOp      (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction2 $ liftComparison (>))
    , ( TBuiltin GTOp      (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction2 $ liftComparison (>))
    , ( TBuiltin GTEOp     (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction2 $ liftComparison (>=))
    , ( TBuiltin GTEOp     (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction2 $ liftComparison (>=))
    , ( TBuiltin GTEOp     (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction2 $ liftComparison (>=))
    , ( TBuiltin GTEOp     (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction2 $ liftComparison (>=))
    , ( TBuiltin LTOp      (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction2 $ liftComparison (<))
    , ( TBuiltin LTOp      (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction2 $ liftComparison (<))
    , ( TBuiltin LTOp      (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction2 $ liftComparison (<))
    , ( TBuiltin LTOp      (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction2 $ liftComparison (<))
    , ( TBuiltin LTEOp     (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction2 $ liftComparison (<=))
    , ( TBuiltin LTEOp     (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction2 $ liftComparison (<=))
    , ( TBuiltin LTEOp     (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction2 $ liftComparison (<=))
    , ( TBuiltin LTEOp     (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction2 $ liftComparison (<=))
    , ( TBuiltin EqualsOp  (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction2 $ liftComparison (==))
    , ( TBuiltin EqualsOp  (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction2 $ liftComparison (==))
    , ( TBuiltin EqualsOp  (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction2 $ liftComparison (==))
    , ( TBuiltin EqualsOp  (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction2 $ liftComparison (==))
    , ( TBuiltin DivOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction2 $ kimaDivision)
    , ( TBuiltin DivOp     (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction2 $ kimaDivision)
    , ( TBuiltin DivOp     (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction2 $ kimaDivision)
    , ( TBuiltin DivOp     (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction2 $ kimaDivision)
    , ( TBuiltin ModOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction2 $ liftIntegralOp mod)
    , ( TBuiltin PrintFunc (KFunc ([KString]        $-> KUnit   )), BuiltinFunction1 kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KInt]           $-> KUnit   )), BuiltinFunction1 kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KFloat]         $-> KUnit   )), BuiltinFunction1 kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KBool]          $-> KUnit   )), BuiltinFunction1 kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KUnit]          $-> KUnit   )), BuiltinFunction1 kimaPrint)
    , ( TBuiltin InputFunc (KFunc ([]               $-> KString )), BuiltinFunction0 (String <$> consoleRead))
    ]

showValue :: Value -> Maybe String
showValue (Integer v)        = Just (show v)
showValue (Float   v)        = Just (show v)
showValue (Bool    v)        = Just (show v)
showValue (String  v)        = Just v
showValue Unit               = Just "()"
showValue Function{}         = Nothing
showValue BuiltinFunction0{} = Nothing
showValue BuiltinFunction1{} = Nothing
showValue BuiltinFunction2{} = Nothing
showValue BuiltinFunction3{} = Nothing

kimaPrint :: (MonadRE m, MonadConsole m) => Value -> m Value
kimaPrint v = case showValue v of
    Just str -> consoleWrite str $> Unit
    Nothing  -> throwError (BuiltinFunctionError "Can't print this value")

kimaDivision :: (MonadRE m) => Value -> Value -> m Value
kimaDivision (Integer l) (Integer r) = return $ Integer (l `div` r)
kimaDivision (Integer l) (Float   r) = return $ Float (fromInteger l / r)
kimaDivision (Float   l) (Integer r) = return $ Float (l / fromInteger r)
kimaDivision (Float   l) (Float   r) = return $ Float (l / r)
kimaDivision l           r           = throwError
    (BuiltinFunctionError ("Can't divide " <> show l <> " and " <> show r))

liftIntegralOp
    :: (MonadRE m)
    => (forall a . Integral a => a -> a -> a)
    -> Value
    -> Value
    -> m Value
liftIntegralOp op (Integer l) (Integer r) = return $ Integer (l `op` r)
liftIntegralOp _  l           r           = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )

liftNumOp
    :: (MonadRE m)
    => (forall a . Num a => a -> a -> a)
    -> Value
    -> Value
    -> m Value
liftNumOp op (Integer l) (Integer r) = return $ Integer (l `op` r)
liftNumOp op (Integer l) (Float   r) = return $ Float (fromInteger l `op` r)
liftNumOp op (Float   l) (Integer r) = return $ Float (l `op` fromInteger r)
liftNumOp op (Float   l) (Float   r) = return $ Float (l `op` r)
liftNumOp _  l           r           = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )

liftComparison
    :: (MonadRE m)
    => (forall a . Ord a => a -> a -> Bool)
    -> Value
    -> Value
    -> m Value
liftComparison op (Integer l) (Integer r) = return $ Bool (l `op` r)
liftComparison op (Integer l) (Float   r) = return $ Bool (fromInteger l `op` r)
liftComparison op (Float   l) (Integer r) = return $ Bool (l `op` fromInteger r)
liftComparison op (Float   l) (Float   r) = return $ Bool (l `op` r)
liftComparison _  l           r           = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )
