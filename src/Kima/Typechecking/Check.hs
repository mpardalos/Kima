module Kima.Typechecking.Check (checkProgram, checkFuncDef, checkStmt, checkExpr) where

import Prelude hiding ( lookup )

import Control.Monad
import Control.Monad.Except
import Kima.Control.Monad.State.Extended

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Kima.AST.Common as AST
import Kima.AST.Parsed as P
import Kima.AST.Typed as T
import Kima.AST.Expression 
import Kima.KimaTypes as KT
import Kima.Typechecking.Monad
import Kima.Typechecking.Types

-- | Check a parsed function definition and return a typed function definition.
-- | It also binds the function name
checkFuncDef :: P.FuncDef -> KTypeM T.FuncDef
checkFuncDef FuncDef {name, signature, body} = do 
    typedSig <- resolveTypeExpr `mapM` signature
    typedBody <- checkFunc typedSig body
    return $ FuncDef name typedSig typedBody

-- | Check a parsed function against a resolved signature and return the typed 
-- | body of the function
checkFunc :: T.NamedSignature -> P.Stmt -> KTypeM T.Stmt
checkFunc sig body = do
    funcEnvironment <- namedSigEnvironment sig

    checkedBody <- withState (<> funcEnvironment) (checkStmt body)

    assertEqualTypes (stmtRetType checkedBody) (AST.returnType sig)

    return checkedBody

------------- Expressions -------------
-- | Check a stmt while applying any effects it has to the typing context (binding new variables etc.)
checkStmt :: P.Stmt -> KTypeM T.Stmt
checkStmt (P.BlockStmt stmts) = checkBlockStmt <$> mapM checkStmt stmts >>= id
checkStmt (P.LetStmt name tExpr expr) = checkLetStmt <$> pure name <*> resolveTypeExpr tExpr <*> checkExpr expr >>= id
checkStmt (P.VarStmt name tExpr expr) = checkVarStmt <$> pure name <*> resolveTypeExpr tExpr <*> checkExpr expr >>= id
checkStmt (P.AssignStmt name expr) = checkAssignStmt <$> pure name <*> checkExpr expr >>= id
checkStmt (P.ExprStmt expr) = checkExprStmt <$> checkExpr expr >>= id
checkStmt (P.WhileStmt cond body) = checkWhileStmt <$> checkExpr cond <*> checkStmt body >>= id
checkStmt (P.IfStmt cond ifBlk elseBlk) = checkIfStmt <$> checkExpr cond <*> checkStmt ifBlk <*> checkStmt elseBlk >>= id

checkBlockStmt :: [T.Stmt] -> KTypeM T.Stmt
checkBlockStmt = return . T.BlockStmt 

checkLetStmt :: Name -> KType -> T.Expr -> KTypeM T.Stmt
checkLetStmt name t expr = do 
    checkBinding Constant name t expr 
    return $ T.LetStmt name t expr

checkVarStmt :: Name -> KType -> T.Expr -> KTypeM T.Stmt
checkVarStmt name t expr = do 
    checkBinding Variable name t expr
    return $ T.VarStmt name t expr

checkAssignStmt :: Name -> T.Expr -> KTypeM T.Stmt
checkAssignStmt name expr = do 
    assertEqualTypes <$> resolveIdentifier name <*> pure (exprType expr) >>= id
    return $ T.AssignStmt name expr

checkExprStmt :: T.Expr -> KTypeM T.Stmt
checkExprStmt e = return $ T.ExprStmt e

checkWhileStmt :: T.Expr -> T.Stmt -> KTypeM T.Stmt
checkWhileStmt cond body = do assertEqualTypes (exprType cond) KBool
                              return $ T.WhileStmt cond body

checkIfStmt :: T.Expr -> T.Stmt -> T.Stmt -> KTypeM T.Stmt
checkIfStmt cond tBranch fBranch = do assertEqualTypes (exprType cond) KBool
                                      assertEqualTypes (stmtRetType tBranch) (stmtRetType fBranch)
                                      return $ T.IfStmt cond tBranch fBranch

------------- Expressions -------------
-- | Check an expression
checkExpr :: P.Expr -> KTypeM T.Expr
checkExpr (P.Identifier name) = checkIdentifier name 
checkExpr (P.FuncExpr sig body) = checkFuncExpr <$> mapM resolveTypeExpr sig <*> pure body >>= id
checkExpr (P.Call callee args) = checkCall <$> checkExpr callee <*> (checkExpr `mapM` args) >>= id
checkExpr (P.LiteralExpr lit) = checkLiteral lit
checkExpr (P.BinExpr binary) = checkBinary =<< mapM checkExpr binary
checkExpr (P.UnaryExpr unary) = checkUnary =<< mapM checkExpr unary

checkIdentifier :: Name -> KTypeM T.Expr
checkIdentifier name = T.Identifier <$> pure name <*> resolveIdentifier name 

checkCall :: T.Expr -> [T.Expr] -> KTypeM T.Expr
checkCall callee args = case exprType callee of
    (KFunc sigs) -> case matchingSignature of
        Just sig -> return $ T.Call callee args (KT.returnType sig)
        Nothing -> throwError $ NoMatchingSignature "__missing_name" argTypes
        where
            argTypes = exprType <$> args
            matchingSignature = find (\sig -> KT.arguments sig == argTypes) sigs
    t -> throwError $ NotAFunctionError t

checkBinary :: Binary T.Expr -> KTypeM T.Expr
checkBinary binary = T.BinExpr binary <$> binaryType (exprType <$> binary)

checkUnary :: Unary T.Expr -> KTypeM T.Expr
checkUnary unary = T.UnaryExpr unary <$> unaryType (exprType <$> unary)

checkLiteral :: Literal -> KTypeM T.Expr
checkLiteral l = return $ T.LiteralExpr l (literalType l)

checkFuncExpr :: T.NamedSignature -> P.Stmt -> KTypeM T.Expr
checkFuncExpr sig body = T.FuncExpr sig <$> checkFunc sig body

------------- Type Expressions -------------

-- |Resolve a TypeExpr in the current typing context
resolveTypeExpr :: TypeExpr -> KTypeM KType
resolveTypeExpr (TypeName name) = Map.lookup name . types <$> getCtx >>= \case
    Just ty -> return ty
    Nothing -> throwError (TypeLookupError name)
resolveTypeExpr (AST.SignatureType args rt) = do 
    sig <- resolveSig args rt
    return $ KFunc [sig]

-- |Lookup all TypeExprs in a parsed signature and return a typed Signature
resolveSig :: [TypeExpr] -> TypeExpr -> KTypeM KT.Signature
resolveSig argTypeExprs returnTypeExpr = do
    returnType <- resolveTypeExpr returnTypeExpr
    argTypes   <- resolveTypeExpr `mapM` argTypeExprs
    return (argTypes $-> returnType)

literalType :: Literal -> KType
literalType (IntExpr _) = KInt
literalType (FloatExpr _) = KFloat
literalType (BoolExpr _) = KBool
literalType (StringExpr _) = KString

binaryType :: MonadError TypeError m => Binary KType -> m KType
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

unaryType :: MonadError TypeError m => Unary KType -> m KType
unaryType (Negate KInt) = return KInt
unaryType (Negate KFloat) = return KFloat
unaryType (Negate eType) = throwError $ UnaryOpTypeError
    [KInt, KFloat] eType
unaryType (Invert KBool) = return KBool
unaryType (Invert eType) = throwError $ UnaryOpTypeError
    [KBool] eType

------------- Helpers -------------
-- |Get the binding of an identifier in the current context. May raise a LookupError
bindingOf :: Name -> KTypeM TypeBinding
bindingOf name = Map.lookup name . bindings <$> getCtx >>= \case
    Just binding -> return binding
    Nothing      -> lookupError name

resolveIdentifier :: Name -> KTypeM KType
resolveIdentifier name = kType <$> bindingOf name

namedSigEnvironment :: T.NamedSignature -> KTypeM TypeCtx
namedSigEnvironment sig = do
    let (argNames, argTypes) = unzip $ AST.arguments sig
    let argBindings = Constant <$> argTypes

    return $ TypeCtx mempty (Map.fromList $ zip argNames argBindings)

getError :: MonadError err m => m a -> m (Maybe err)
getError action = (const Nothing <$> action) `catchError` (return . Just)

checkBinding :: (KType -> TypeBinding) -> Name -> KType -> T.Expr -> KTypeM ()
-- If bindingOf throws an error then it the name is not bound and we can proceed
checkBinding bind name expectedType expr = getError (bindingOf name) >>= \case
    Just (LookupError _) -> do
        assertEqualTypes expectedType (exprType expr)
        bindName (bind expectedType) name
    _ -> throwError $ NameAlreadyBoundError name

envFromList :: [(Name, TypeBinding)] -> Map Name TypeBinding
envFromList = Map.fromListWith comb
    where
        comb :: TypeBinding -> TypeBinding -> TypeBinding
        comb Constant { kType = KFunc sigl } Constant { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
        comb Constant { kType = KFunc sigl } Variable { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
        comb Variable { kType = KFunc sigl } Constant { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
        comb Variable { kType = KFunc sigl } Variable { kType = KFunc sigr } = Variable $ KFunc (sigl ++ sigr)
        comb _ r = r

resolveNamedSig :: P.NamedSignature -> KTypeM KT.Signature
resolveNamedSig NamedSignature { arguments, returnType=returnTypeExpr } =
    Signature <$> argTypes <*> returnType
    where
        argTypes :: KTypeM [KType]
        argTypes = mapM resolveTypeExpr (snd <$> arguments)

        returnType :: KTypeM KType
        returnType = resolveTypeExpr returnTypeExpr
        

checkProgram :: P.Program -> KTypeM T.Program
checkProgram (P.Program funcDefs) = do 
    funcSignatures <- forM funcDefs $ \funcDef -> do 
        typedSig <- resolveNamedSig (signature funcDef)
        return (name funcDef, Constant $ KFunc [typedSig])

    let hoistedCtx = TypeCtx mempty (envFromList funcSignatures)
    T.Program <$> withState (<> hoistedCtx) (mapM checkFuncDef funcDefs)
