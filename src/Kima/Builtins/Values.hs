{-# LANGUAGE OverloadedLists #-}
module Kima.Builtins.Values where

import           Safe
import           Control.Monad.Except
import           Data.Functor
import           Data.Map                       ( Map )

import           Kima.AST
import           Kima.Interpreter.Types

printStringOperation = KOperation "print" [KString] KUnit
printIntOperation = KOperation "print" [KInt] KUnit
printFloatOperation = KOperation "print" [KFloat] KUnit
printBoolOperation = KOperation "print" [KBool] KUnit
printUnitOperation = KOperation "print" [KUnit] KUnit
inputOperation = KOperation "input" [] KString

baseEnv :: Map RuntimeIdentifier Value
baseEnv =
    [ ( TBuiltin AddOp (KFunc [KInt, KInt] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (+)
      )
    , ( TBuiltin AddOp (KFunc [KInt, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (+)
      )
    , ( TBuiltin AddOp (KFunc [KFloat, KInt] PureEffect KFloat)
      , BuiltinFunction $ liftNumOp (+)
      )
    , ( TBuiltin AddOp (KFunc [KFloat, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (+)
      )
    , ( TBuiltin AddOp (KFunc [KString, KString] PureEffect KString)
      , BuiltinFunction kimaStrConcat
      )
    , ( TBuiltin SubOp (KFunc [KInt, KInt] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (-)
      )
    , ( TBuiltin SubOp (KFunc [KInt, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (-)
      )
    , ( TBuiltin SubOp (KFunc [KFloat, KInt] PureEffect KFloat)
      , BuiltinFunction $ liftNumOp (-)
      )
    , ( TBuiltin SubOp (KFunc [KFloat, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (-)
      )
    , ( TBuiltin MulOp (KFunc [KInt, KInt] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (*)
      )
    , ( TBuiltin MulOp (KFunc [KInt, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (*)
      )
    , ( TBuiltin MulOp (KFunc [KFloat, KInt] PureEffect KFloat)
      , BuiltinFunction $ liftNumOp (*)
      )
    , ( TBuiltin MulOp (KFunc [KFloat, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftNumOp (*)
      )
    , ( TBuiltin PowOp (KFunc [KInt, KInt] PureEffect KInt)
      , BuiltinFunction $ liftFloatingOp (**)
      )
    , ( TBuiltin PowOp (KFunc [KInt, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftFloatingOp (**)
      )
    , ( TBuiltin PowOp (KFunc [KFloat, KInt] PureEffect KFloat)
      , BuiltinFunction $ liftFloatingOp (**)
      )
    , ( TBuiltin PowOp (KFunc [KFloat, KFloat] PureEffect KInt)
      , BuiltinFunction $ liftFloatingOp (**)
      )
    , ( TBuiltin GTOp (KFunc [KInt, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>)
      )
    , ( TBuiltin GTOp (KFunc [KInt, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>)
      )
    , ( TBuiltin GTOp (KFunc [KFloat, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>)
      )
    , ( TBuiltin GTOp (KFunc [KFloat, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>)
      )
    , ( TBuiltin GTEOp (KFunc [KInt, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>=)
      )
    , ( TBuiltin GTEOp (KFunc [KInt, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>=)
      )
    , ( TBuiltin GTEOp (KFunc [KFloat, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>=)
      )
    , ( TBuiltin GTEOp (KFunc [KFloat, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (>=)
      )
    , ( TBuiltin LTOp (KFunc [KInt, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<)
      )
    , ( TBuiltin LTOp (KFunc [KInt, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<)
      )
    , ( TBuiltin LTOp (KFunc [KFloat, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<)
      )
    , ( TBuiltin LTOp (KFunc [KFloat, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<)
      )
    , ( TBuiltin LTEOp (KFunc [KInt, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<=)
      )
    , ( TBuiltin LTEOp (KFunc [KInt, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<=)
      )
    , ( TBuiltin LTEOp (KFunc [KFloat, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<=)
      )
    , ( TBuiltin LTEOp (KFunc [KFloat, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (<=)
      )
    , ( TBuiltin EqualsOp (KFunc [KInt, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (==)
      )
    , ( TBuiltin EqualsOp (KFunc [KInt, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (==)
      )
    , ( TBuiltin EqualsOp (KFunc [KFloat, KInt] PureEffect KBool)
      , BuiltinFunction $ liftComparison (==)
      )
    , ( TBuiltin EqualsOp (KFunc [KFloat, KFloat] PureEffect KBool)
      , BuiltinFunction $ liftComparison (==)
      )
    , ( TBuiltin EqualsOp (KFunc [KString, KString] PureEffect KBool)
      , BuiltinFunction stringEquality
      )
    , ( TBuiltin DivOp (KFunc [KInt, KInt] PureEffect KInt)
      , BuiltinFunction kimaDivision
      )
    , ( TBuiltin DivOp (KFunc [KInt, KFloat] PureEffect KInt)
      , BuiltinFunction kimaDivision
      )
    , ( TBuiltin DivOp (KFunc [KFloat, KInt] PureEffect KFloat)
      , BuiltinFunction kimaDivision
      )
    , ( TBuiltin DivOp (KFunc [KFloat, KFloat] PureEffect KInt)
      , BuiltinFunction kimaDivision
      )
    , ( TBuiltin ModOp (KFunc [KInt, KInt] PureEffect KInt)
      , BuiltinFunction $ liftIntegralOp mod
      )
    , ( TBuiltin NegateOp (KFunc [KInt] PureEffect KInt)
      , BuiltinFunction kimaNegate
      )
    , ( TBuiltin NegateOp (KFunc [KFloat] PureEffect KFloat)
      , BuiltinFunction kimaNegate
      )
    , ( TBuiltin InvertOp (KFunc [KBool] PureEffect KBool)
      , BuiltinFunction kimaInvert
      )
    , ( TBuiltin
          PrintFunc
          (KFunc [KString] (AnonymousEffect [printStringOperation]) KUnit)
      , BuiltinFunction kimaPrint
      )
    , ( TBuiltin PrintFunc
                 (KFunc [KInt] (AnonymousEffect [printIntOperation]) KUnit)
      , BuiltinFunction kimaPrint
      )
    , ( TBuiltin
          PrintFunc
          (KFunc [KFloat] (AnonymousEffect [printFloatOperation]) KUnit)
      , BuiltinFunction kimaPrint
      )
    , ( TBuiltin
          PrintFunc
          (KFunc [KBool] (AnonymousEffect [printBoolOperation]) KUnit)
      , BuiltinFunction kimaPrint
      )
    , ( TBuiltin
          PrintFunc
          (KFunc [KUnit] (AnonymousEffect [printUnitOperation]) KUnit)
      , BuiltinFunction kimaPrint
      )
    , ( TBuiltin InputFunc (KFunc [] (AnonymousEffect [inputOperation]) KString)
      , BuiltinFunction (\_ -> String <$> consoleRead)
      )
    , ( TIdentifier "at" (KFunc [KInt, KString] PureEffect KString)
      , BuiltinFunction kimaAt
      )
    , ( TIdentifier "panic" (KFunc [KString] PureEffect KUnit)
      , BuiltinFunction kimaPanic
      )
    , ( TIdentifier "length" (KFunc [KString] PureEffect KInt)
      , BuiltinFunction kimaLength
      )
    ]

showValue :: Value -> Maybe String
showValue (Integer v    )   = Just (show v)
showValue (Float   v    )   = Just (show v)
showValue (Bool    True )   = Just "True"
showValue (Bool    False)   = Just "False"
showValue (String  v    )   = Just v
showValue Unit              = Just "()"
showValue Function{}        = Nothing
showValue BuiltinFunction{} = Nothing
showValue ProductData{}     = Nothing
showValue AccessorIdx{}     = Nothing

kimaPrint :: (MonadRE m, MonadConsole m) => [Value] -> m Value
kimaPrint [v] = case showValue v of
    Just str -> consoleWrite str $> Unit
    Nothing  -> throwError (BuiltinFunctionError "Can't print this value")
kimaPrint _ =
    throwError (BuiltinFunctionError "print only accepts one argument")

kimaInvert :: MonadRE m => [Value] -> m Value
kimaInvert [Bool b] = return (Bool $ not b)
kimaInvert v = throwError (BuiltinFunctionError ("Can't negate " <> show v))


kimaStrConcat :: MonadRE m => [Value] -> m Value
kimaStrConcat [String str1, String str2] = pure (String (str1 <> str2))
kimaStrConcat [l          , r          ] = throwError
    (BuiltinFunctionError ("Can't add " <> show l <> " and " <> show r))
kimaStrConcat _ =
    throwError (BuiltinFunctionError "Wrong argument count for (+)")

kimaDivision :: (MonadRE m) => [Value] -> m Value
kimaDivision [Integer l, Integer r] = return $ Integer (l `div` r)
kimaDivision [Integer l, Float r  ] = return $ Float (fromInteger l / r)
kimaDivision [Float   l, Integer r] = return $ Float (l / fromInteger r)
kimaDivision [Float   l, Float r  ] = return $ Float (l / r)
kimaDivision [l        , r        ] = throwError
    (BuiltinFunctionError ("Can't divide " <> show l <> " and " <> show r))
kimaDivision _ =
    throwError (BuiltinFunctionError "Wrong argument count for (/)")

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

liftFloatingOp
    :: (MonadRE m)
    => (forall a . (RealFrac a, Floating a) => a -> a -> a)
    -> ([Value] -> m Value)
liftFloatingOp op [Integer l, Integer r] =
    return $ Integer (truncate ((fromIntegral l `op` fromIntegral r) :: Double))
liftFloatingOp op [Integer l, Float r  ] = return $ Float (fromInteger l `op` r)
liftFloatingOp op [Float   l, Integer r] = return $ Float (l `op` fromInteger r)
liftFloatingOp op [Float   l, Float r  ] = return $ Float (l `op` r)
liftFloatingOp _  [l        , r        ] = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )
liftFloatingOp _ _ = throwError
    (BuiltinFunctionError "Wrong argument count for numerical operator")

liftNumOp
    :: (MonadRE m) => (forall a . Num a => a -> a -> a) -> ([Value] -> m Value)
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
liftComparison op [Integer l, Float r  ] = return $ Bool (fromInteger l `op` r)
liftComparison op [Float   l, Integer r] = return $ Bool (l `op` fromInteger r)
liftComparison op [Float   l, Float r  ] = return $ Bool (l `op` r)
liftComparison _  [l        , r        ] = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )
liftComparison _ _ = throwError
    (BuiltinFunctionError "Wrong argument count for comparison operator")

stringEquality :: MonadRE m => [Value] -> m Value
stringEquality [String l, String r] = return $ Bool (l == r)
stringEquality [l        , r        ] = throwError
    (BuiltinFunctionError
        ("Can't apply operation to " <> show l <> " and " <> show r)
    )
stringEquality _ = throwError
    (BuiltinFunctionError "Wrong argument count for comparison operator")

kimaNegate :: MonadRE m => [Value] -> m Value
kimaNegate [Float   v] = return $ Float (-v)
kimaNegate [Integer v] = return $ Integer (-v)
kimaNegate [l] = throwError (BuiltinFunctionError ("Can't negate " <> show l))
kimaNegate _ =
    throwError (BuiltinFunctionError "Wrong argument count for negation")

kimaAt :: MonadRE m => [Value] -> m Value
kimaAt [Integer n, String xs] = case xs `atMay` fromIntegral n of
    Just c -> return (String [c])
    Nothing ->
        throwError (BuiltinFunctionError ("String has no element " <> show n))
kimaAt _ = throwError (BuiltinFunctionError "Wrong arguments for 'at'")

kimaLength :: MonadRE m => [Value] -> m Value
kimaLength [String xs] = return (Integer (fromIntegral $ length xs))
kimaLength _ = throwError (BuiltinFunctionError "Wrong arguments for 'length'")

kimaPanic :: MonadRE m => [Value] -> m Value
kimaPanic [String msg] = throwError (BuiltinFunctionError msg)
kimaPanic _ = throwError (BuiltinFunctionError "Wrong arguments for 'error'")
