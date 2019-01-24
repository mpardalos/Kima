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
baseTypeCtx = TypeCtx {
    bindings=Map.foldlWithKey combine Map.empty (unEnv baseEnv),
    typeBindings=
        [ ("String", KString)
        , ("Unit", KUnit)
        , ("Int", KInt)
        , ("Float", KFloat)
        , ("Bool", KBool)
        ]
}
  where
    combine typeCtx name _ =
        Map.insertWith (<>) (deTypeAnnotate name) (Binding Constant [nameType name]) typeCtx

baseEnv :: Environment Value
baseEnv = Environment
    [ ( TBuiltin AddOp     (KFunc ([KInt, KInt]       $-> KInt    )), BuiltinFunction $ liftNumOp (+))
    , ( TBuiltin AddOp     (KFunc ([KInt, KFloat]     $-> KInt    )), BuiltinFunction $ liftNumOp (+))
    , ( TBuiltin AddOp     (KFunc ([KFloat, KInt]     $-> KFloat  )), BuiltinFunction $ liftNumOp (+))
    , ( TBuiltin AddOp     (KFunc ([KFloat, KFloat]   $-> KInt    )), BuiltinFunction $ liftNumOp (+))
    , ( TBuiltin AddOp     (KFunc ([KString, KString] $-> KString)), BuiltinFunction kimaStrConcat)
    , ( TBuiltin SubOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction $ liftNumOp (-))
    , ( TBuiltin SubOp     (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction $ liftNumOp (-))
    , ( TBuiltin SubOp     (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction $ liftNumOp (-))
    , ( TBuiltin SubOp     (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction $ liftNumOp (-))
    , ( TBuiltin MulOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction $ liftNumOp (*))
    , ( TBuiltin MulOp     (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction $ liftNumOp (*))
    , ( TBuiltin MulOp     (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction $ liftNumOp (*))
    , ( TBuiltin MulOp     (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction $ liftNumOp (*))
    , ( TBuiltin GTOp      (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction $ liftComparison (>))
    , ( TBuiltin GTOp      (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction $ liftComparison (>))
    , ( TBuiltin GTOp      (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction $ liftComparison (>))
    , ( TBuiltin GTOp      (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction $ liftComparison (>))
    , ( TBuiltin GTEOp     (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction $ liftComparison (>=))
    , ( TBuiltin GTEOp     (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction $ liftComparison (>=))
    , ( TBuiltin GTEOp     (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction $ liftComparison (>=))
    , ( TBuiltin GTEOp     (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction $ liftComparison (>=))
    , ( TBuiltin LTOp      (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction $ liftComparison (<))
    , ( TBuiltin LTOp      (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction $ liftComparison (<))
    , ( TBuiltin LTOp      (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction $ liftComparison (<))
    , ( TBuiltin LTOp      (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction $ liftComparison (<))
    , ( TBuiltin LTEOp     (KFunc ([KInt, KInt]     $-> KBool   )), BuiltinFunction $ liftComparison (<=))
    , ( TBuiltin LTEOp     (KFunc ([KInt, KFloat]   $-> KBool   )), BuiltinFunction $ liftComparison (<=))
    , ( TBuiltin LTEOp     (KFunc ([KFloat, KInt]   $-> KBool   )), BuiltinFunction $ liftComparison (<=))
    , ( TBuiltin LTEOp     (KFunc ([KFloat, KFloat] $-> KBool   )), BuiltinFunction $ liftComparison (<=))
    , ( TBuiltin EqualsOp  (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction $ liftComparison (==))
    , ( TBuiltin EqualsOp  (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction $ liftComparison (==))
    , ( TBuiltin EqualsOp  (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction $ liftComparison (==))
    , ( TBuiltin EqualsOp  (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction $ liftComparison (==))
    , ( TBuiltin DivOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction $ kimaDivision)
    , ( TBuiltin DivOp     (KFunc ([KInt, KFloat]   $-> KInt    )), BuiltinFunction $ kimaDivision)
    , ( TBuiltin DivOp     (KFunc ([KFloat, KInt]   $-> KFloat  )), BuiltinFunction $ kimaDivision)
    , ( TBuiltin DivOp     (KFunc ([KFloat, KFloat] $-> KInt    )), BuiltinFunction $ kimaDivision)
    , ( TBuiltin ModOp     (KFunc ([KInt, KInt]     $-> KInt    )), BuiltinFunction $ liftIntegralOp mod)
    , ( TBuiltin NegateOp  (KFunc ([KInt] $-> KInt     )), BuiltinFunction $ kimaNegate)
    , ( TBuiltin NegateOp  (KFunc ([KFloat] $-> KFloat )), BuiltinFunction $ kimaNegate)
    , ( TBuiltin InvertOp  (KFunc ([KBool] $-> KBool )), BuiltinFunction $ kimaInvert)
    , ( TBuiltin PrintFunc (KFunc ([KString] $-> KUnit )), BuiltinFunction kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KInt]    $-> KUnit )), BuiltinFunction kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KFloat]  $-> KUnit )), BuiltinFunction kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KBool]   $-> KUnit )), BuiltinFunction kimaPrint)
    , ( TBuiltin PrintFunc (KFunc ([KUnit]   $-> KUnit )), BuiltinFunction kimaPrint)
    , ( TBuiltin InputFunc (KFunc ([] $-> KString )), BuiltinFunction (\_ -> String <$> consoleRead))
    ]

showValue :: Value -> Maybe String
showValue (Integer v)       = Just (show v)
showValue (Float   v)       = Just (show v)
showValue (Bool    v)       = Just (show v)
showValue (String  v)       = Just v
showValue Unit              = Just "()"
showValue Function{}        = Nothing
showValue BuiltinFunction{} = Nothing
showValue ProductData{}     = Nothing

kimaPrint :: (MonadRE m, MonadConsole m) => [Value] -> m Value
kimaPrint [v] = case showValue v of
    Just str -> consoleWrite str $> Unit
    Nothing  -> throwError (BuiltinFunctionError "Can't print this value")
kimaPrint _ = 
    throwError (BuiltinFunctionError "print only accepts one argument")

kimaInvert :: MonadRE m => [Value] -> m Value
kimaInvert [Bool b] = return (Bool $ not b)
kimaInvert v = throwError
    (BuiltinFunctionError ("Can't negate " <> show v))


kimaStrConcat :: MonadRE m => [Value] -> m Value
kimaStrConcat [String str1, String str2] = pure (String (str1 <> str2))
kimaStrConcat [l, r]             = throwError
    (BuiltinFunctionError ("Can't add " <> show l <> " and " <> show r))
kimaStrConcat _                  = throwError
    (BuiltinFunctionError "Wrong argument count for (+)")

kimaDivision :: (MonadRE m) => [Value] -> m Value
kimaDivision [Integer l, Integer r] = return $ Integer (l `div` r)
kimaDivision [Integer l, Float   r] = return $ Float (fromInteger l / r)
kimaDivision [Float   l, Integer r] = return $ Float (l / fromInteger r)
kimaDivision [Float   l, Float   r] = return $ Float (l / r)
kimaDivision [l,          r]        = throwError
    (BuiltinFunctionError ("Can't divide " <> show l <> " and " <> show r))
kimaDivision _        = throwError (BuiltinFunctionError "Wrong argument count for (/)")

liftIntegralOp
    :: (MonadRE m)
    => (forall a . Integral a => a -> a -> a)
    -> ([Value] -> m Value)
liftIntegralOp op [Integer l, Integer r] = return $ Integer (l `op` r)
liftIntegralOp _  [l        , r        ] = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )
liftIntegralOp _ _ = throwError
    (BuiltinFunctionError "Wrong argument count for numerical operator")

liftNumOp
    :: (MonadRE m) => (forall a . Num a => a -> a -> a) 
    -> ([Value] -> m Value)
liftNumOp op [Integer l, Integer r] = return $ Integer (l `op` r)
liftNumOp op [Integer l, Float r  ] = return $ Float (fromInteger l `op` r)
liftNumOp op [Float   l, Integer r] = return $ Float (l `op` fromInteger r)
liftNumOp op [Float   l, Float r  ] = return $ Float (l `op` r)
liftNumOp _  [l        , r        ] = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )
liftNumOp _ _ = throwError
    (BuiltinFunctionError "Wrong argument count for numerical operator")

liftComparison
    :: MonadRE m
    => (forall a . Ord a => a -> a -> Bool)
    -> ([Value] -> m Value)
liftComparison op [Integer l, Integer r] = return $ Bool (l `op` r)
liftComparison op [Integer l, Float   r] = return $ Bool (fromInteger l `op` r)
liftComparison op [Float   l, Integer r] = return $ Bool (l `op` fromInteger r)
liftComparison op [Float   l, Float   r] = return $ Bool (l `op` r)
liftComparison _  [l, r]           = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )
liftComparison _ _ = throwError
    (BuiltinFunctionError "Wrong argument count for comparison operator")

kimaNegate :: MonadRE m => [Value] -> m Value
kimaNegate [Float v] = return $ Float (-v)
kimaNegate [Integer v] = return $ Integer (-v)
kimaNegate [l]           = throwError
    (BuiltinFunctionError ("Can't negate " <> show l))
kimaNegate _ = throwError
    (BuiltinFunctionError "Wrong argument count for negation")
