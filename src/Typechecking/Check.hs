module Typechecking.Check where

import           Prelude                 hiding ( lookup )
import qualified Prelude                       as P
                                                ( lookup )
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Extended
import           Data.Comp.Algebra
import           Data.Comp.Sum

import           Data.Map
import           Safe

import           Typechecking.Types            as TC
import           Typechecking.Monad            as TC
import           AST

------------- Statements -------------
checkStmt :: Stmt -> KTypeM KType
checkStmt (LetStmt name tExpr expr) = KUnit <$ checkBinding Constant name tExpr expr
checkStmt (VarStmt name tExpr expr) =
    KUnit <$ checkBinding Variable name tExpr expr
checkStmt (AssignStmt name expr) =
    KUnit <$ (assertEqualTypes <$> checkExpr expr <*> typeOf name)
checkStmt (ExprStmt expr) = checkExpr expr
checkStmt (WhileStmt cond body) = do 
    assertEqualTypes KBool =<< checkExpr cond
    checkBlock body
    return KUnit


------------- Expressions -------------
checkExpr :: Expr -> KTypeM KType
checkExpr = cataM checkAlgebraM

type TypeCheckAlg f = Alg f KType
type TypeCheckAlgM f = AlgM KTypeM f KType

class TypeCheckable f where
    checkAlgebraM :: TypeCheckAlgM f

instance (TypeCheckable f, TypeCheckable g) => TypeCheckable (f :+: g) where
    checkAlgebraM = caseF 
        (checkAlgebraM :: TypeCheckAlgM f)
        (checkAlgebraM :: TypeCheckAlgM g)

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
    checkAlgebraM (Add _ _) = throwError _

    checkAlgebraM (Sub KInt KInt)     = return KInt
    checkAlgebraM (Sub KFloat KFloat) = return KFloat
    checkAlgebraM (Sub KFloat KInt)   = return KFloat
    checkAlgebraM (Sub KInt KFloat)   = return KFloat
    checkAlgebraM (Sub _ _) = throwError _

    checkAlgebraM (Mul KInt KInt)     = return KInt
    checkAlgebraM (Mul KFloat KFloat) = return KFloat
    checkAlgebraM (Mul KFloat KInt)   = return KFloat
    checkAlgebraM (Mul KInt KFloat)   = return KFloat
    checkAlgebraM (Mul _ _) = throwError _

    checkAlgebraM (Div KInt KInt)     = return KInt
    checkAlgebraM (Div KFloat KFloat) = return KFloat
    checkAlgebraM (Div KFloat KInt)   = return KFloat
    checkAlgebraM (Div KInt KFloat)   = return KFloat
    checkAlgebraM (Div _ _) = throwError _

    checkAlgebraM (Mod KInt KInt)     = return KInt
    checkAlgebraM (Mod KFloat KFloat) = return KFloat
    checkAlgebraM (Mod KFloat KInt)   = return KFloat
    checkAlgebraM (Mod KInt KFloat)   = return KFloat
    checkAlgebraM (Mod _ _) = throwError _

instance TypeCheckable UnaryExpr where
    checkAlgebraM (Negate KInt) = return KInt
    checkAlgebraM (Negate KFloat) = return KFloat
    checkAlgebraM (Negate _) = _

    checkAlgebraM (Invert KBool) = return KBool
    checkAlgebraM (Invert _) = _
    
instance TypeCheckable FuncExpr where
    checkAlgebraM (FuncExpr sig body) = KFunc <$> checkFunc sig body

instance TypeCheckable Call where
    checkAlgebraM (CallExpr calleeType argTypes) = case calleeType of
        KFunc Signature { arguments = expectedArgTypes, returnType } -> do
            let argCount         = toInteger $ length argTypes
            let expectedArgCount = toInteger $ length expectedArgTypes
            assert (argCount == expectedArgCount)
                (ArgumentCountError expectedArgCount argCount)
            zipWithM_ assertEqualTypes expectedArgTypes argTypes
            return returnType
        t -> notAFunctionError t

------------- Helpers -------------
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
