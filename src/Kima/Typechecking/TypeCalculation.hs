module Kima.Typechecking.TypeCalculation(
    exprType, stmtRetType, nameType, untypedName
)where

import Control.Monad.Except

import Data.List

import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.Types

import Safe

exprType :: MonadError (TypeError KTypeOv) m => TypedAST 'Expr -> m KTypeOv
exprType (LiteralE lit) = return $ literalType lit
exprType (Identifier name) = return $ nameType name
exprType (FuncExpr args body) = do 
    rt <- stmtRetType body
    return $ KFuncOv [(nameType <$> args) $-> rt]
exprType (Call callee args) = exprType callee >>= \case
    (KFuncOv sigs) -> (matchingSignature <$> pure sigs <*> getArgTypes) >>= \case 
        Just sig -> return $ returnType sig
        Nothing -> throwError . NoMatchingSignature "__missing_name" =<< getArgTypes
        where getArgTypes = mapM exprType args
    t -> throwError (NotAFunctionError t)
exprType (BinE bin) = mapM exprType bin >>= binaryType
exprType (UnaryE unary) = mapM exprType unary >>= unaryType

stmtRetType :: MonadError (TypeError KTypeOv) m => TypedAST 'Stmt -> m KTypeOv
stmtRetType (ExprStmt expr) = exprType expr
stmtRetType (Block s) = case lastMay s of
    Just stmt -> stmtRetType stmt
    Nothing -> return KUnit
stmtRetType (If IfStmt { ifBlk }) = stmtRetType ifBlk
stmtRetType (While _) = return KUnit
stmtRetType (Assign _ _) = return KUnit

matchingSignature :: [Signature KTypeOv] -> [KTypeOv] -> Maybe (Signature KTypeOv)
matchingSignature sigs argTypes = find ((argTypes ==) . arguments) sigs

nameType :: TypedName -> KTypeOv
nameType (TypedName _ t) = t

untypedName :: TypedName -> ParsedName
untypedName (TypedName n _) = Name n

literalType :: Literal -> KType ov
literalType (IntExpr _) = KInt
literalType (FloatExpr _) = KFloat
literalType (BoolExpr _) = KBool
literalType (StringExpr _) = KString

binaryType :: MonadError (TypeError KTypeOv) m => Binary KTypeOv -> m KTypeOv
binaryType (Add KFloat KFloat) = return KFloat
binaryType (Add KFloat KInt)   = return KFloat
binaryType (Add KInt KFloat)   = return KFloat
binaryType (Add lType rType)   = throwError $ BinOpTypeError 
    [ (KInt, KInt)
    , (KFloat, KFloat)
    , (KFloat, KInt)
    , (KInt, KFloat)
    ] (lType, rType)

binaryType (Sub KInt KInt)     = return KInt
binaryType (Sub KFloat KFloat) = return KFloat
binaryType (Sub KFloat KInt)   = return KFloat
binaryType (Sub KInt KFloat)   = return KFloat
binaryType (Sub lType rType)   = throwError $ BinOpTypeError 
    [ (KInt, KInt)
    , (KFloat, KFloat)
    , (KFloat, KInt)
    , (KInt, KFloat)
    ] (lType, rType)

binaryType (Mul KInt KInt)     = return KInt
binaryType (Mul KFloat KFloat) = return KFloat
binaryType (Mul KFloat KInt)   = return KFloat
binaryType (Mul KInt KFloat)   = return KFloat
binaryType (Mul lType rType)   = throwError $ BinOpTypeError 
    [ (KInt, KInt)
    , (KFloat, KFloat)
    , (KFloat, KInt)
    , (KInt, KFloat)
    ] (lType, rType)

binaryType (Div KInt KInt)     = return KInt
binaryType (Div KFloat KFloat) = return KFloat
binaryType (Div KFloat KInt)   = return KFloat
binaryType (Div KInt KFloat)   = return KFloat
binaryType (Div lType rType)   = throwError $ BinOpTypeError 
    [ (KInt, KInt)
    , (KFloat, KFloat)
    , (KFloat, KInt)
    , (KInt, KFloat)
    ] (lType, rType)

binaryType (Mod KInt KInt)     = return KInt
binaryType (Mod KFloat KFloat) = return KFloat
binaryType (Mod KFloat KInt)   = return KFloat
binaryType (Mod KInt KFloat)   = return KFloat
binaryType (Mod lType rType)   = throwError $ BinOpTypeError 
    [ (KInt, KInt)
    , (KFloat, KFloat)
    , (KFloat, KInt)
    , (KInt, KFloat)
    ] (lType, rType)

unaryType :: MonadError (TypeError KTypeOv) m => Unary KTypeOv -> m KTypeOv
unaryType (Negate KInt) = return KInt
unaryType (Negate KFloat) = return KFloat
unaryType (Negate eType) = throwError $ UnaryOpTypeError
    [KInt, KFloat] eType
unaryType (Invert KBool) = return KBool
unaryType (Invert eType) = throwError $ UnaryOpTypeError
    [KBool] eType