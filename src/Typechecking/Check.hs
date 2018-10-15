module Typechecking.Check where

import           Prelude                 hiding ( lookup )
import qualified Prelude                       as P
                                                ( lookup )
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Extended

import           Data.Map
import           Safe

import           Typechecking.Types            as TC
import           Typechecking.Monad            as TC
import           AST

-- |Get the binding of an identifier in the current context. May raise a LookupError
bindingOf :: Name -> KTypeM TypeBinding
bindingOf name = lookup name . bindings <$> getCtx >>= \case
    Just binding -> return binding
    Nothing      -> lookupError name

-- |Get the type of an identifier in the current context. May raise a LookupError
typeOf :: Name -> KTypeM KType
typeOf name = kType <$> bindingOf name

namedSigEnvironment :: AST.NamedSignature -> KTypeM TypeCtx
namedSigEnvironment sig = do
    typedSig <- resolveNamedSig sig

    let argNames    = fst <$> AST.arguments sig
    let argBindings = Constant <$> TC.arguments typedSig
    let bindings    = fromList (zip argNames argBindings)

    return $ TypeCtx mempty bindings


resolveNamedSig :: AST.NamedSignature -> KTypeM TC.Signature
resolveNamedSig NamedSignature { arguments, returnType } =
    resolveSig (snd <$> arguments) returnType

-- |Lookup all TypeExprs in a parsed signature and return a typed Signature
resolveSig :: [TypeExpr] -> TypeExpr -> KTypeM TC.Signature
resolveSig argTypeExprs returnTypeExpr = do
    returnType <- resolveTypeExpr returnTypeExpr
    argTypes   <- resolveTypeExpr `mapM` argTypeExprs
    return (argTypes $-> returnType)

-- |Resolve a TypeExpr in the current typing context
resolveTypeExpr :: TypeExpr -> KTypeM KType
resolveTypeExpr (TypeName name) = lookup name . types <$> getCtx >>= \case
    Just ty -> return ty
    Nothing -> throwError (TypeLookupError name)
resolveTypeExpr (AST.SignatureType args rt) = KFunc <$> resolveSig args rt

expectedBinOpTypes :: BinOp -> [((KType, KType), KType)]
expectedBinOpTypes Add =
    [ ((KInt, KInt)    , KInt)
    , ((KFloat, KFloat), KFloat)
    , ((KFloat, KInt)  , KFloat)
    , ((KInt, KFloat)  , KFloat)
    ]
expectedBinOpTypes Sub =
    [ ((KInt, KInt)    , KInt)
    , ((KFloat, KFloat), KFloat)
    , ((KFloat, KInt)  , KFloat)
    , ((KInt, KFloat)  , KFloat)
    ]
expectedBinOpTypes Div =
    [ ((KInt, KInt)    , KInt)
    , ((KFloat, KFloat), KFloat)
    , ((KFloat, KInt)  , KFloat)
    , ((KInt, KFloat)  , KFloat)
    ]
expectedBinOpTypes Mul =
    [ ((KInt, KInt)    , KInt)
    , ((KFloat, KFloat), KFloat)
    , ((KFloat, KInt)  , KFloat)
    , ((KInt, KFloat)  , KFloat)
    ]
expectedBinOpTypes Mod =
    [ ((KInt, KInt)    , KInt)
    , ((KFloat, KFloat), KFloat)
    , ((KFloat, KInt)  , KFloat)
    , ((KInt, KFloat)  , KFloat)
    ]

expectedUnaryOpTypes :: UnaryOp -> [(KType, KType)]
expectedUnaryOpTypes Negate = [(KInt, KInt), (KFloat, KFloat)]
expectedUnaryOpTypes Invert = [(KBool, KBool)]

checkExpr :: Expr -> KTypeM KType
checkExpr (IntExpr    _        ) = return KInt
checkExpr (FloatExpr  _        ) = return KFloat
checkExpr (BoolExpr   _        ) = return KBool
checkExpr (StringExpr _        ) = return KString
checkExpr (FuncExpr sig    body) = KFunc <$> checkFunc sig body
checkExpr (CallExpr callee args) = checkExpr callee >>= \case
    KFunc TC.Signature { arguments = expectedArgTypes, returnType } -> do
        let argCount         = toInteger $ length args
        let expectedArgCount = toInteger $ length expectedArgTypes
        assert (argCount == expectedArgCount)
               (ArgumentCountError expectedArgCount argCount)
        argTypes <- mapM checkExpr args
        zipWithM_ assertEqualTypes expectedArgTypes argTypes
        return returnType
    t -> notAFunctionError t
checkExpr (UnaryExpr op expr) = do
    exprType <- checkExpr expr
    let typeOptions = expectedUnaryOpTypes op
    case P.lookup exprType typeOptions of
        Just rt -> return rt
        Nothing -> throwError $ UnaryOpTypeError (fst <$> typeOptions) exprType
checkExpr (BinExpr op l r) = do
    lType <- checkExpr l
    rType <- checkExpr r
    let typeOptions = expectedBinOpTypes op
    case P.lookup (lType, rType) typeOptions of
        Just rt -> return rt
        Nothing ->
            throwError $ BinOpTypeError (fst <$> typeOptions) (lType, rType)
checkExpr (IdentifierExpr name) = typeOf name

checkStmt :: Stmt -> KTypeM KType
checkStmt (LetStmt name tExpr expr) =
    KUnit <$ checkBinding Constant name tExpr expr
checkStmt (VarStmt name tExpr expr) =
    KUnit <$ checkBinding Variable name tExpr expr
checkStmt (AssignStmt name expr) =
    KUnit <$ (assertEqualTypes <$> checkExpr expr <*> typeOf name)
checkStmt (ExprStmt expr) = checkExpr expr
checkStmt (WhileStmt cond body) = do 
    assertEqualTypes KBool =<< checkExpr cond
    checkBlock body
    return KUnit

checkBinding :: (KType -> TypeBinding) -> Name -> TypeExpr -> Expr -> KTypeM ()
checkBinding bind name tExpr expr = do
    expectedType <- resolveTypeExpr tExpr
    exprType     <- checkExpr expr
    assertEqualTypes expectedType exprType
    bindName (bind exprType) name

-- |Typecheck a block, return its return type
checkBlock :: Block -> KTypeM KType
checkBlock (Block stmts) = fromMaybe KUnit <$> lastMay <$> mapM checkStmt stmts

checkFunc :: NamedSignature -> Block -> KTypeM TC.Signature
checkFunc sig body = do
    typedSig        <- resolveNamedSig sig
    funcEnvironment <- namedSigEnvironment sig

    bodyRetType     <- withState (const funcEnvironment) (checkBlock body)
    assertEqualTypes bodyRetType (TC.returnType typedSig)

    return typedSig

checkFuncDef :: FuncDef -> KTypeM ()
checkFuncDef FuncDef {name, signature, body} = do 
    typedSig <- checkFunc signature body
    bindName (Constant . KFunc $ typedSig) name

-- |Typecheck a block, return its return type
checkProgram :: Program -> KTypeM ()
checkProgram (Program ast) = mapM_ checkFuncDef ast
