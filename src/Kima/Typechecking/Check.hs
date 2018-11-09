module Kima.Typechecking.Check where

import Prelude hiding ( lookup )

import Control.Monad
import Control.Monad.Except

import Data.Comp.Algebra
import Data.Comp.Sum

import Data.Map (Map)
import qualified Data.Map as Map

import Kima.AST as AST
import Kima.Control.Monad.State.Extended
import Kima.KimaTypes as T
import Kima.Typechecking.Monad
import Kima.Typechecking.Types

import Safe


type TypeCheckAlg f = Alg f KType
type TypeCheckAlgM f = AlgM KTypeM f KType

class TypeCheckable f where
    checkAlgebraM :: TypeCheckAlgM f

instance (TypeCheckable f, TypeCheckable g) => TypeCheckable (f :+: g) where
    checkAlgebraM = caseF 
        (checkAlgebraM :: TypeCheckAlgM f)
        (checkAlgebraM :: TypeCheckAlgM g)

------------- Statements -------------
checkStmt :: Stmt -> KTypeM KType
checkStmt (Stmt stmt) = cataM checkAlgebraM stmt

instance TypeCheckable BlockStmt where
    checkAlgebraM (BlockStmt returnTypes) = return (lastDef KUnit returnTypes)

instance TypeCheckable (QualifiedAssignment Expr) where
    checkAlgebraM (LetStmt name tExpr expr) = 
        KUnit <$ checkBinding Constant name tExpr expr
    checkAlgebraM (VarStmt name tExpr expr) =
        KUnit <$ checkBinding Variable name tExpr expr
    checkAlgebraM (AssignStmt name expr) =
        KUnit <$ (assertEqualTypes <$> checkExpr expr <*> typeOf name)

instance TypeCheckable (ExprStmt Expr) where
    checkAlgebraM (ExprStmt expr) = checkExpr expr

instance TypeCheckable (WhileLoop Expr) where
    checkAlgebraM (WhileStmt cond _) = do 
        _ <- assertEqualTypes <$> checkExpr cond <*> pure KBool
        return KUnit

instance TypeCheckable (IfStmt Expr) where
    checkAlgebraM (IfStmt cond ifblk elseblk) = do 
        condType <- checkExpr cond

        assertEqualTypes condType KBool
        assertEqualTypes ifblk elseblk
        return ifblk

------------- Expressions -------------
checkExpr :: Expr -> KTypeM KType
checkExpr (Expr expr) = cataM checkAlgebraM expr

instance TypeCheckable Literal where
    checkAlgebraM (IntExpr _)    = return KInt
    checkAlgebraM (FloatExpr _)  = return KFloat
    checkAlgebraM (BoolExpr _)   = return KBool
    checkAlgebraM (StringExpr _) = return KString

instance TypeCheckable Identifier where
    checkAlgebraM (IdentifierExpr name) = typeOf name

instance TypeCheckable BinExpr where
    checkAlgebraM (Add KInt KInt)     = return KInt
    checkAlgebraM (Add KFloat KFloat) = return KFloat
    checkAlgebraM (Add KFloat KInt)   = return KFloat
    checkAlgebraM (Add KInt KFloat)   = return KFloat
    checkAlgebraM (Add lType rType)   = throwError $ BinOpTypeError 
        [ (KInt, KInt)
        , (KFloat, KFloat)
        , (KFloat, KInt)
        , (KInt, KFloat)
        ] (lType, rType)

    checkAlgebraM (Sub KInt KInt)     = return KInt
    checkAlgebraM (Sub KFloat KFloat) = return KFloat
    checkAlgebraM (Sub KFloat KInt)   = return KFloat
    checkAlgebraM (Sub KInt KFloat)   = return KFloat
    checkAlgebraM (Sub lType rType)   = throwError $ BinOpTypeError 
        [ (KInt, KInt)
        , (KFloat, KFloat)
        , (KFloat, KInt)
        , (KInt, KFloat)
        ] (lType, rType)

    checkAlgebraM (Mul KInt KInt)     = return KInt
    checkAlgebraM (Mul KFloat KFloat) = return KFloat
    checkAlgebraM (Mul KFloat KInt)   = return KFloat
    checkAlgebraM (Mul KInt KFloat)   = return KFloat
    checkAlgebraM (Mul lType rType)   = throwError $ BinOpTypeError 
        [ (KInt, KInt)
        , (KFloat, KFloat)
        , (KFloat, KInt)
        , (KInt, KFloat)
        ] (lType, rType)

    checkAlgebraM (Div KInt KInt)     = return KInt
    checkAlgebraM (Div KFloat KFloat) = return KFloat
    checkAlgebraM (Div KFloat KInt)   = return KFloat
    checkAlgebraM (Div KInt KFloat)   = return KFloat
    checkAlgebraM (Div lType rType)   = throwError $ BinOpTypeError 
        [ (KInt, KInt)
        , (KFloat, KFloat)
        , (KFloat, KInt)
        , (KInt, KFloat)
        ] (lType, rType)

    checkAlgebraM (Mod KInt KInt)     = return KInt
    checkAlgebraM (Mod KFloat KFloat) = return KFloat
    checkAlgebraM (Mod KFloat KInt)   = return KFloat
    checkAlgebraM (Mod KInt KFloat)   = return KFloat
    checkAlgebraM (Mod lType rType)   = throwError $ BinOpTypeError 
        [ (KInt, KInt)
        , (KFloat, KFloat)
        , (KFloat, KInt)
        , (KInt, KFloat)
        ] (lType, rType)

instance TypeCheckable UnaryExpr where
    checkAlgebraM (Negate KInt) = return KInt
    checkAlgebraM (Negate KFloat) = return KFloat
    checkAlgebraM (Negate eType) = throwError $ UnaryOpTypeError
        [KInt, KFloat] eType

    checkAlgebraM (Invert KBool) = return KBool
    checkAlgebraM (Invert eType) = throwError $ UnaryOpTypeError
        [KBool] eType
    
instance TypeCheckable (FuncExpr Stmt) where
    checkAlgebraM (FuncExpr sig body) = do
        checkedSig <- checkFunc sig body
        return $ KFunc [checkedSig]

instance TypeCheckable Call where
    checkAlgebraM (CallExpr calleeType argTypes) = checkCall calleeType argTypes

checkCall :: KType -> [KType] -> KTypeM KType
checkCall (KFunc sigs) args = foldl @[] checkSig (throwError $ NoMatchingSignature "__missing_name" args) sigs
    where 
        checkSig :: KTypeM KType -> Signature -> KTypeM KType
        checkSig _   sig | T.arguments sig == args = return $ T.returnType sig
        checkSig def _                              = def
checkCall callee _= throwError $ NotAFunctionError callee

------------- Helpers -------------
-- |Get the binding of an identifier in the current context. May raise a LookupError
bindingOf :: Name -> KTypeM TypeBinding
bindingOf name = Map.lookup name . bindings <$> getCtx >>= \case
    Just binding -> return binding
    Nothing      -> lookupError name

-- |Get the type of an identifier in the current context. May raise a LookupError
typeOf :: Name -> KTypeM KType
typeOf name = kType <$> bindingOf name

namedSigEnvironment :: AST.NamedSignature -> KTypeM TypeCtx
namedSigEnvironment sig = do
    typedSig <- resolveNamedSig sig

    let argNames    = fst <$> AST.arguments sig
    let argBindings = Constant <$> T.arguments typedSig
    let bindings    = Map.fromList (zip argNames argBindings)

    return $ TypeCtx mempty bindings


resolveNamedSig :: AST.NamedSignature -> KTypeM T.Signature
resolveNamedSig NamedSignature { arguments, returnType } =
    resolveSig (snd <$> arguments) returnType

-- |Lookup all TypeExprs in a parsed signature and return a typed Signature
resolveSig :: [TypeExpr] -> TypeExpr -> KTypeM T.Signature
resolveSig argTypeExprs returnTypeExpr = do
    returnType <- resolveTypeExpr returnTypeExpr
    argTypes   <- resolveTypeExpr `mapM` argTypeExprs
    return (argTypes $-> returnType)

-- |Resolve a TypeExpr in the current typing context
resolveTypeExpr :: TypeExpr -> KTypeM KType
resolveTypeExpr (TypeName name) = Map.lookup name . types <$> getCtx >>= \case
    Just ty -> return ty
    Nothing -> throwError (TypeLookupError name)
resolveTypeExpr (AST.SignatureType args rt) = do 
    sig <- resolveSig args rt
    return $ KFunc [sig]

checkBinding :: (KType -> TypeBinding) -> Name -> TypeExpr -> Expr -> KTypeM ()
checkBinding bind name tExpr expr = do
    expectedType <- resolveTypeExpr tExpr
    exprType     <- checkExpr expr
    assertEqualTypes expectedType exprType
    bindName (bind exprType) name

checkFunc :: NamedSignature -> Stmt -> KTypeM T.Signature
checkFunc sig body = do
    typedSig        <- resolveNamedSig sig
    funcEnvironment <- namedSigEnvironment sig

    bodyRetType     <- withState (<> funcEnvironment) (checkStmt body)
    assertEqualTypes bodyRetType (T.returnType typedSig)

    return typedSig

checkFuncDef :: FuncDef Stmt -> KTypeM ()
checkFuncDef FuncDef {name, signature, body} = do 
    typedSig <- checkFunc signature body
    bindName (Constant . KFunc $ [typedSig]) name

envFromList :: [(Name, TypeBinding)] -> Map Name TypeBinding
envFromList = Map.fromListWith comb
    where
        comb :: TypeBinding -> TypeBinding -> TypeBinding
        comb Constant { kType = KFunc sigl } Constant { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
        comb Constant { kType = KFunc sigl } Variable { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
        comb Variable { kType = KFunc sigl } Constant { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
        comb Variable { kType = KFunc sigl } Variable { kType = KFunc sigr } = Variable $ KFunc (sigl ++ sigr)
        comb _ r = r

-- |Typecheck a block, return its return type
checkProgram :: Program Stmt -> KTypeM ()
checkProgram (Program funcDefs) = do 
    funcSignatures <- forM funcDefs $ \funcDef -> do 
        typedSig <- resolveNamedSig (signature funcDef)
        return (name funcDef, Constant $ KFunc [typedSig])
    let hoistedCtx = TypeCtx mempty (envFromList funcSignatures)
    withState (<> hoistedCtx) (mapM_ checkFuncDef funcDefs)
