module Kima.Typechecking.Check (check) where

import Prelude hiding ( lookup )

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Kima.Control.Monad.State.Extended

import Data.Bitraversable
import Data.List

import qualified Data.Map as Map

import Kima.AST as AST
import Kima.KimaTypes as KT
import Kima.Typechecking.Monad
import Kima.Typechecking.Types
import Kima.Typechecking.TypeCalculation

-- | Check a parsed function definition and return a typed function definition.
-- | It also binds the function name
check :: ParsedAST p -> KTypeM (TypedAST p)
check (Program funcDefs) = do 
    hoistedCtx <- TypeCtx mempty . Map.fromList <$> funcTypes
    Program <$> withState (<> hoistedCtx) 
        (mapM check funcDefs)
    where
        funcTypes :: KTypeM [(ParsedName, TypeBinding)]
        funcTypes = forM funcDefs $ \(FuncDefAnn (Name name) args rtExpr _) -> do
            typedArgs <- mapM resolveTypeExpr (snd <$> args)
            returnType <- resolveTypeExpr rtExpr
    
            return (Name name, Constant $ KFuncOv [typedArgs $-> returnType])

check (FuncDefAnn (Name name) sig rtExpr body) = do
    typedArgs <- getTypedArgs
    returnType <- resolveTypeExpr rtExpr
    typedBody <- checkFunc typedArgs returnType body
    return $ FuncDef (TypedName name (KFuncOv [(nameType <$> typedArgs) $-> returnType])) typedArgs typedBody
    where
        getTypedArgs :: KTypeM [TypedName]
        getTypedArgs = mapM resolveNameType (uncurry (flip typeAnnotate) <$> sig)

        resolveNameType :: GenericName ('Just TypeExpr) 'False -> KTypeM TypedName
        resolveNameType (TypedName thisName thisType) = TypedName thisName <$> resolveTypeExpr thisType
check (FuncExprAnn sig rtExpr body) = do
    typedArgs <- getTypedArgs
    returnType <- resolveTypeExpr rtExpr
    typedBody <- checkFunc typedArgs returnType body
    return $ FuncExpr typedArgs typedBody
    where
        getTypedArgs :: KTypeM [TypedName]
        getTypedArgs = mapM resolveNameType (uncurry (flip typeAnnotate) <$> sig)

        resolveNameType :: GenericName ('Just TypeExpr) 'False -> KTypeM TypedName
        resolveNameType (TypedName thisName thisType) = TypedName thisName <$> resolveTypeExpr thisType

check (Block stmts) = Block <$> mapM check stmts
check (Let name tExpr expr) = checkLetStmt <$> pure name <*> resolveTypeExpr tExpr <*> check expr >>= id
check (Var name tExpr expr) = checkVarStmt <$> pure name <*> resolveTypeExpr tExpr <*> check expr >>= id
check (Assign name expr) = checkAssignStmt <$> pure name <*> check expr >>= id
check (ExprStmt expr) = ExprStmt <$> check expr
check (While stmt) = (checkWhileStmt <=< bimapM check check) stmt
check (If stmt) = (checkIfStmt <=< bimapM check check) stmt
check (Identifier name@(Name nameStr)) = Identifier <$> (TypedName nameStr <$> resolveIdentifier name)
check (Call callee args) = checkCall <$> check callee <*> mapM check args >>= id
check (LiteralE lit) = return $ LiteralE lit
check (BinE binary) = do 
    result <- BinE <$> mapM check binary
    _ <- exprType result -- exprType checks if the types match, so just run it for the check and discard
    return result
check (UnaryE unary) = do 
    result <- UnaryE <$> mapM check unary
    _ <- exprType result
    return result


-- | Check a parsed function against a resolved signature and return the typed 
-- | body of the function
checkFunc :: [TypedName] -> KTypeOv -> ParsedAST 'Stmt -> KTypeM (TypedAST 'Stmt)
checkFunc args rt body = do
    checkedBody <- withState (<> namedSigEnvironment args) 
        (check body)

    assertEqualTypes <$> stmtRetType checkedBody <*> pure rt >>= id

    return checkedBody

checkLetStmt :: ParsedName -> KTypeOv -> TypedAST 'Expr -> KTypeM (TypedAST 'Stmt)
checkLetStmt (Name name) t expr = do 
    checkBinding Constant (Name name) t expr 
    return $ Assign (TypedName name t) expr

checkVarStmt :: ParsedName -> KTypeOv -> TypedAST 'Expr -> KTypeM (TypedAST 'Stmt)
checkVarStmt (Name name) t expr = do 
    checkBinding Variable (Name name) t expr 
    return $ Assign (TypedName name t) expr

checkAssignStmt :: ParsedName -> TypedAST 'Expr -> KTypeM (TypedAST 'Stmt)
checkAssignStmt (Name name) expr = do 
    idType <- resolveIdentifier (Name name)
    assertEqualTypes idType =<< exprType expr
    return $ Assign (TypedName name idType) expr

checkWhileStmt :: WhileStmt (TypedAST 'Expr) (TypedAST 'Stmt) -> KTypeM (TypedAST 'Stmt)
checkWhileStmt (WhileStmt cond body) = do
    assertEqualTypes <$> exprType cond <*> pure KBool >>= id
    return $ While (WhileStmt cond body)

checkIfStmt :: IfStmt (TypedAST 'Expr) (TypedAST 'Stmt)  -> KTypeM (TypedAST 'Stmt)
checkIfStmt IfStmt { cond, ifBlk, elseBlk } = do
    assertEqualTypes <$> exprType cond <*> pure KBool >>= id
    assertEqualTypes <$> stmtRetType ifBlk <*> stmtRetType elseBlk >>= id
    return $ If (IfStmt cond ifBlk elseBlk)

checkCall :: TypedAST 'Expr -> [TypedAST 'Expr] -> KTypeM (TypedAST 'Expr)
checkCall callee args = exprType callee >>= \case
    (KFuncOv sigs) -> do 
        argTypes <- mapM exprType args
        let matchingSignature = find (\sig -> KT.arguments sig == argTypes) sigs

        case matchingSignature of
            Just _ -> return (Call callee args)
            Nothing -> throwError (NoMatchingSignature "__missing_name" argTypes)
    t -> throwError $ NotAFunctionError t

-- ------------- Type Expressions -------------

-- |Resolve a TypeExpr in the current typing context
resolveTypeExpr :: TypeExpr -> KTypeM KTypeOv
resolveTypeExpr (TypeName name) = Map.lookup name . types <$> get >>= \case
    Just ty -> return ty
    Nothing -> throwError (TypeLookupError name)
resolveTypeExpr (SignatureType args rt) = do
    sig <- resolveSig args rt
    return $ KFuncOv [sig]

-- |Lookup all TypeExprs in a parsed signature and return a typed Signature
resolveSig :: [TypeExpr] -> TypeExpr -> KTypeM (KT.Signature KTypeOv)
resolveSig argTypeExprs returnTypeExpr = do
    returnType <- resolveTypeExpr returnTypeExpr
    argTypes   <- resolveTypeExpr `mapM` argTypeExprs
    return (argTypes $-> returnType)


------------- Helpers -------------
-- |Get the binding of an identifier in the current context. May raise a LookupError
bindingOf :: ParsedName -> KTypeM TypeBinding
bindingOf name = Map.lookup name . bindings <$> get >>= \case
    Just binding -> return binding
    Nothing      -> lookupError name

resolveIdentifier :: ParsedName -> KTypeM KTypeOv
resolveIdentifier name = kType <$> bindingOf name

namedSigEnvironment :: [TypedName] -> TypeCtx
namedSigEnvironment sig = TypeCtx 
    mempty 
    (Map.fromList ((untypedName &&& Constant . nameType) <$> sig))

getError :: MonadError err m => m a -> m (Maybe err)
getError action = (const Nothing <$> action) `catchError` (return . Just)

checkBinding :: (KTypeOv -> TypeBinding) -> ParsedName -> KTypeOv -> TypedAST 'Expr -> KTypeM ()
-- If bindingOf throws an error then it the name is not bound and we can proceed
checkBinding bind name expectedType expr = getError (bindingOf name) >>= \case
    Just (LookupError _) -> do
        assertEqualTypes expectedType =<< exprType expr
        bindName (bind expectedType) name
    _ -> throwError $ NameAlreadyBoundError name

-- envFromList :: [(ParsedName, TypeBinding)] -> Map ParsedName TypeBinding
-- envFromList = Map.fromListWith comb
--     where
--         comb :: TypeBinding -> TypeBinding -> TypeBinding
--         comb Constant { kType = KFunc sigl } Constant { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
--         comb Constant { kType = KFunc sigl } Variable { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
--         comb Variable { kType = KFunc sigl } Constant { kType = KFunc sigr } = Constant $ KFunc (sigl ++ sigr)
--         comb Variable { kType = KFunc sigl } Variable { kType = KFunc sigr } = Variable $ KFunc (sigl ++ sigr)
--         comb _ r = r
