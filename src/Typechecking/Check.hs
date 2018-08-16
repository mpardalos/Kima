module Typechecking.Check where

import Prelude hiding (lookup)
import qualified Prelude as P (lookup)
import Data.Maybe
import Control.Monad
import Control.Monad.Except

import Data.Map
import Safe

import Typechecking.BaseTypes
import Frontend.BaseTypes(Expr(..))
import qualified Frontend.BaseTypes as P
import CommonTypes

-- |Get the binding of an identifier in the current context. May raise a LookupError
bindingOf :: Name -> KTypeM TypeBinding
bindingOf name = lookup name . bindings <$> getCtx >>= \case
    Just binding -> return binding
    Nothing -> lookupError name

-- |Get the type of an identifier in the current context. May raise a LookupError
typeOf :: Name -> KTypeM KType
typeOf = (kType <$>) . bindingOf

-- |Lookup all TypeExprs in a parsed signature and return a typed Signature
resolveSig :: P.Signature -> KTypeM Signature
resolveSig P.Signature {P.arguments=argExprs, P.returnType=returnTypeExpr} = do
    returnType <- resolveTypeExpr returnTypeExpr
    argTypes <- (resolveTypeExpr . fst) `mapM` (P.unArgList argExprs)
    return (argTypes $-> returnType)

-- |Resolve a TypeExpr in the current typing context
resolveTypeExpr :: P.TypeExpr -> KTypeM KType
resolveTypeExpr (P.TypeName name) = lookup name . types <$> getCtx >>= \case
    Just ty -> return ty
    Nothing -> lookupError name
resolveTypeExpr (P.SignatureType sig) = KFunc <$> resolveSig sig

expectedOpTypes :: BinOp -> [((KType, KType), KType)]
expectedOpTypes Add = [ ((KInt  , KInt), KInt  ) , ((KFloat, KFloat), KFloat)
                      , ((KFloat, KInt), KFloat) , ((KInt  , KFloat), KFloat) ]
expectedOpTypes Sub = [ ((KInt  , KInt), KInt  ) , ((KFloat, KFloat), KFloat)
                      , ((KFloat, KInt), KFloat) , ((KInt  , KFloat), KFloat) ]
expectedOpTypes Div = [ ((KInt  , KInt), KInt  ) , ((KFloat, KFloat), KFloat)
                      , ((KFloat, KInt), KFloat) , ((KInt  , KFloat), KFloat) ]
expectedOpTypes Mul = [ ((KInt  , KInt), KInt  ) , ((KFloat, KFloat), KFloat)
                      , ((KFloat, KInt), KFloat) , ((KInt  , KFloat), KFloat) ]
expectedOpTypes Mod = [ ((KInt  , KInt), KInt  ) , ((KFloat, KFloat), KFloat)
                      , ((KFloat, KInt), KFloat) , ((KInt  , KFloat), KFloat) ]

checkExpr :: P.Expr -> KTypeM KType
checkExpr (IntExpr _) = return KInt
checkExpr (FloatExpr _) = return KFloat
checkExpr (BoolExpr _) = return KBool
checkExpr (FuncExpr sig _) = KFunc <$> resolveSig sig
checkExpr (CallExpr callee args) = checkExpr callee >>= \case
    KFunc Signature { arguments = expectedArgTypes, returnType } -> do
        let argCount = toInteger $ length args
        let expectedArgCount = toInteger $ length expectedArgTypes
        assert (argCount == expectedArgCount) (ArgumentCountError expectedArgCount argCount)
        argTypes <- mapM checkExpr args
        zipWithM_ assertEqualTypes expectedArgTypes argTypes
        return returnType
    t -> notAFunctionError t
checkExpr (BinExpr op l r) = do
    lType <- checkExpr l
    rType <- checkExpr r
    let typeOptions = expectedOpTypes op
    case P.lookup (lType, rType) typeOptions of 
        Just rt -> return rt
        Nothing -> throwError $ BinOpTypeError (fst <$> typeOptions) (lType, rType)
checkExpr (IdentifierExpr name) = typeOf name

checkStmt :: P.Stmt -> KTypeM KType
checkStmt (P.LetStmt name tExpr expr) = do
    expectedType <- resolveTypeExpr tExpr
    exprType <- checkExpr expr
    assertEqualTypes expectedType exprType
    bindName (Constant exprType) name
    return KUnit
checkStmt (P.VarStmt name tExpr expr) = do
    expectedType <- resolveTypeExpr tExpr
    exprType <- checkExpr expr
    assertEqualTypes expectedType exprType
    bindName (Variable exprType) name
    return KUnit
checkStmt (P.ExprStmt expr) = checkExpr expr
checkStmt (P.WhileStmt cond body) = do 
    condType <- checkExpr cond
    assertEqualTypes KBool condType
    void $ checkBlock body
    return KUnit

-- |Typecheck a block, return its return type
checkBlock :: P.Block -> KTypeM KType
checkBlock (P.Block stmts) = fromMaybe KUnit <$> lastMay <$> mapM checkStmt stmts 