{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.ConstraintGen
    ( makeConstraints
    , ConstraintGenerationError
    )
where

import           Control.Applicative
import           Control.Arrow                 as Arrow
                                         hiding ( first
                                                , second
                                                )
import           Control.Monad.Writer
import           Control.Monad.Except
import           Data.Bifunctor                as Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Safe

import           Kima.Typechecking.Types
import           Kima.AST
import           Kima.Control.Monad.State.Extended
import           Kima.KimaTypes
import           Kima.Typechecking.Builtins

------------------------- Evaluation ------------------------------------------------
-- Anything inside here should not be used in the rest of the module

newtype ConstraintGenerator a = ConstraintGenerator (
    StateT (TypeCtx, Int) (
    WriterT SomeConstraintSet (
    Either ConstraintGenerationError)) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadWriter SomeConstraintSet,
        MonadError ConstraintGenerationError)

instance MonadState TypeCtx ConstraintGenerator where
    get     = ConstraintGenerator . StateT $ \(ctx, tvar) -> pure (ctx, (ctx, tvar))
    put ctx = ConstraintGenerator . StateT $ \(_,   tvar) -> pure ((), (ctx, tvar))

instance ApplicativeSupply TypeVar ConstraintGenerator where
    supply = ConstraintGenerator . StateT $ \(ctx, tvar) -> pure (TypeVar tvar, (ctx, tvar+1))

-- | Try to assign a type to every identifier in an AST, and give the constraints between those identifiers
makeConstraints
    :: DesugaredAST 'TopLevel
    -> Either
           ConstraintGenerationError
           (TVarAST 'TopLevel, SomeConstraintSet)
makeConstraints ast = runConstraintGenerator $ do
    annotatedAST     <- resolveTypes ast
    annotatedTVarAST <- addTVar annotatedAST         -- Add type variables 
    writeProgramConstraints annotatedTVarAST           -- Generate constraints
    return (removeTypeAnnotations annotatedTVarAST)  -- Remove type annotations

runConstraintGenerator
    :: ConstraintGenerator a
    -> Either ConstraintGenerationError (a, SomeConstraintSet)
runConstraintGenerator (ConstraintGenerator cg) =
    cg & (flip evalStateT (baseCtx, 0) >>> runWriterT)

-------------------------------------------------------------------------------------

-------------- Constraint generation Monad -----------------------------------------------
data ConstraintGenerationError = UnboundName TVarName
                               | NameShadowed TVarName
                               | TypeResolutionError TypeExpr
    deriving Show

class Applicative m => ApplicativeSupply s m | m -> s where
    -- Law:
    -- liftA2 (==) supply supply == pure False
    supply :: m s

type MonadConstraintWriter m = MonadWriter SomeConstraintSet m
type MonadConstraintGenerator m = (MonadConstraintWriter m, MonadState TypeCtx m, MonadError ConstraintGenerationError m)
type ApplicativeTVarSupply m = ApplicativeSupply TypeVar m
type AnnotatedTVarAST p = AST p 'NoSugar TVarName ('Just KType)

writeConstraint c = tell [SomeConstraint c]
newTVar :: ApplicativeTVarSupply m => m TypeVar
newTVar = supply
withNewTVar :: ApplicativeTVarSupply m => DesugaredName -> m TVarName
withNewTVar = liftA2 typeAnnotate newTVar . pure
addTVar
    :: ApplicativeTVarSupply m => TypeAnnotatedAST p -> m (AnnotatedTVarAST p)
addTVar = traverseNames withNewTVar
---------------------------------------------------------------------------

--------------------- Resolving TypeExprs ----------------------------------
type TypeAnnotatedAST p = AST p 'NoSugar DesugaredName ('Just KType)

resolveTypes
    :: MonadError ConstraintGenerationError m
    => DesugaredAST p
    -> m (TypeAnnotatedAST p)
resolveTypes = traverseTypeAnnotations resolveTypeExpr

-- | Resolves a type expression. Since custom type don't exist yet, 
-- | just has a hardcoded list of types
resolveTypeExpr :: MonadError ConstraintGenerationError m => TypeExpr -> m KType
resolveTypeExpr (TypeName "Int"   )             = pure KInt
resolveTypeExpr (TypeName "String")             = pure KString
resolveTypeExpr (TypeName "Float" )             = pure KFloat
resolveTypeExpr (TypeName "Bool"  )             = pure KBool
resolveTypeExpr (TypeName "Unit"  )             = pure KUnit
resolveTypeExpr tExpr@TypeName{} = throwError (TypeResolutionError tExpr)
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return (KFunc (args $-> rt))
---------------------------------------------------------------------------

--------------------- Making constraints ----------------------------------

-- | Generate constraints for a top-level AST
writeProgramConstraints
    :: MonadConstraintGenerator m => AnnotatedTVarAST 'TopLevel -> m ()
writeProgramConstraints (Program funcDefs) = withState
    (<> hoistedCtx)
    (traverse_ writeFuncDefConstraints funcDefs)
  where
    hoistedCtx :: TypeCtx
    hoistedCtx = Map.fromListWith (<>) (funcTypeBinding <$> funcDefs)

    funcTypeBinding
        :: AnnotatedTVarAST 'FunctionDef -> (DesugaredName, Set KType)
    funcTypeBinding (FuncDefAnn name args rt _) =
        (deTypeAnnotate name, [KFunc ((snd <$> args) $-> rt)])

-- | Write the constraints for a function definition
writeFuncDefConstraints
    :: MonadConstraintGenerator m => AnnotatedTVarAST 'FunctionDef -> m ()
writeFuncDefConstraints (FuncDefAnn name args rt body) = do
    funcType <- functionType args rt body
    writeConstraint $ nameType name `IsOneOf` [funcType]
    traverse_ -- Write the domains of the arguments
        ((nameType *** Set.singleton) >>> uncurry IsOneOf >>> writeConstraint)
        args
    modify (Map.insertWith (<>) (deTypeAnnotate name) [funcType])

-- | Compute the return type of a statement and write the constraints required
-- | for typing it
stmtReturnTVar
    :: MonadConstraintGenerator m => AnnotatedTVarAST 'Stmt -> m TypeVar
stmtReturnTVar (ExprStmt expr) = do
    tvar <- exprTVar expr
    writeConstraint (tvar =#= tvar)
    return tvar
stmtReturnTVar (Block blk) =
    lastDef (TheType KUnit) <$> traverse stmtReturnTVar blk
stmtReturnTVar (While (WhileStmt cond body)) = do
    condT  <- exprTVar cond
    _bodyT <- withState id (stmtReturnTVar body)

    writeConstraint $ condT =#= TheType KBool

    pure $ TheType KUnit
stmtReturnTVar (If (IfStmt cond ifBlk elseBlk)) = do
    condT    <- exprTVar cond
    ifBlkT   <- withState id (stmtReturnTVar ifBlk)
    elseBlkT <- withState id (stmtReturnTVar elseBlk)

    writeConstraint $ condT =#= TheType KBool
    writeConstraint $ ifBlkT =#= elseBlkT

    pure $ TheType KUnit
stmtReturnTVar (Var name declaredTyped expr) = do
    checkLocalAssignment name declaredTyped expr
    pure (TheType KUnit)
stmtReturnTVar (Let name t expr) = do
    checkLocalAssignment name t expr
    pure (TheType KUnit)
stmtReturnTVar (Assign name expr) = do
    currentNameTypes <- gets $ Map.lookup (deTypeAnnotate name)
    exprType         <- exprTVar expr
    case currentNameTypes of
        Just ts -> writeConstraint (exprType `IsOneOf` ts)
        Nothing -> throwError (UnboundName name)
    pure $ TheType KUnit

-- | Compute the type of an expression and write the constraints required for
-- | typing it
exprTVar :: MonadConstraintGenerator m => AnnotatedTVarAST 'Expr -> m TypeVar
exprTVar (LiteralE   l     ) = pure (TheType $ literalType l)
exprTVar (Identifier idName) = do
    -- It must have one of the types that this name has in this scope
    let nameT = nameType idName
    typesInScope <- fromMaybe [] <$> gets (Map.lookup (deTypeAnnotate idName))
    writeConstraint $ nameT `IsOneOf` typesInScope
    pure nameT
exprTVar (FuncExprAnn args rt body) = TheType <$> functionType args rt body
exprTVar (Call callee args        ) = do
    calleeT <- exprTVar callee
    argT    <- traverse exprTVar args
    pure (ApplicationTVar calleeT argT)

functionType
    :: MonadConstraintGenerator m
    => [(TVarName, KType)]
    -> KType
    -> AnnotatedTVarAST 'Stmt
    -> m KType
functionType args rt body = do
    let argCtx = Map.fromList (bimap deTypeAnnotate Set.singleton <$> args)

    returnTVar <- withState (<> argCtx) (stmtReturnTVar body)
    writeConstraint $ returnTVar =#= TheType rt

    pure . KFunc $ ((snd <$> args) $-> rt)

-- | Assert that the declared type of an assignment matches the actual type
-- | as well as that the binding does not shadow another name. Shadowing is 
-- | only allowed at the top level.
checkLocalAssignment
    :: MonadConstraintGenerator m
    => TVarName
    -> KType
    -> AnnotatedTVarAST 'Expr
    -> m ()
checkLocalAssignment name declaredType expr = do
    exprType <- exprTVar expr
    writeConstraint $ nameType name `IsOneOf` [declaredType]
    gets (Map.lookup (deTypeAnnotate name)) >>= \case
        Just _  -> throwError (NameShadowed name)
        Nothing -> do
            writeConstraint $ nameType name =#= TheType declaredType
            writeConstraint $ exprType =#= TheType declaredType
            modify (Map.insert (deTypeAnnotate name) [declaredType])

literalType :: Literal -> KType
literalType IntExpr{}    = KInt
literalType FloatExpr{}  = KFloat
literalType BoolExpr{}   = KBool
literalType StringExpr{} = KString
-------------------------------------- ------------------------------------------------------------------
