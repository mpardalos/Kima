{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonadComprehensions #-}
module Kima.Typechecking.ConstraintGen where

import           Kima.Typechecking.Types

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Writer
import           Data.Bifunctor                as Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

import           Safe

import           Kima.AST
import           Kima.Control.Monad.State.Extended
import           Kima.KimaTypes

-- TODO: BaseCtx

makeConstraints
    :: DesugaredAST 'TopLevel
    -> Maybe (TVarAST 'TopLevel, ConstraintSet)
makeConstraints =
    resolveTypes                                                 -- Resolve types
        >>> fmap
                (\annotatedAST -> do
                    annotatedTVarAST <- addTVar annotatedAST         -- Add type variables 
                    writeProgramConstraints annotatedTVarAST           -- Generate constraints
                    return (removeTypeAnnotations annotatedTVarAST)  -- Remove type annotations
                )
        >>> fmap runConstraintGenerator                          -- Extract everything

-------------- For supply of type variables -------------------
class Monad m => MonadUniqueSupply s m | m -> s where
    -- Law:
    -- liftA2 (==) supply supply == pure False
    supply :: m s

instance MonadUniqueSupply TypeVar ConstraintGenerator where
    supply = ConstraintGenerator . StateT $ \(ctx, tvar) -> pure (TypeVar tvar, (ctx, tvar+1))
---------------------------------------------------------------

-------------- Constraint generation Monad -----------------------------------------------
type MonadConstraintWriter m = MonadWriter ConstraintSet m
type MonadConstraintGenerator m = (MonadConstraintWriter m, MonadState TypeCtx m)

-- For now, when a name is declared, it has to have an associated type, so this
-- type is OK. If type inference is implemented to any degree, this will have to
-- be changed to map to a [TypeVar]
type TypeCtx = Map DesugaredName [KType]

newtype ConstraintGenerator a = ConstraintGenerator (StateT (TypeCtx, Int) (Writer ConstraintSet) a)
    deriving (Functor, Applicative, Monad, MonadWriter ConstraintSet)

instance MonadState TypeCtx ConstraintGenerator where
    get     = ConstraintGenerator . StateT $ \(ctx, tvar) -> pure (ctx, (ctx, tvar))
    put ctx = ConstraintGenerator . StateT $ \(_, tvar)   -> pure ((), (ctx, tvar))

runConstraintGenerator :: ConstraintGenerator a -> (a, ConstraintSet)
runConstraintGenerator (ConstraintGenerator cg) =
    cg & (flip evalStateT (mempty, 0) >>> runWriter)

newTVar :: MonadTVarSupply m => m TypeVar
newTVar = supply
writeConstraint :: MonadConstraintWriter m => Constraint -> m ()
writeConstraint c = tell [c]
-------------------------------------------------------------------------------------------

--------------------- Resolving TypeExprs ----------------------------------
type TypeAnnotatedAST p = AST p 'NoSugar DesugaredName ('Just KType)

resolveTypes :: DesugaredAST p -> Maybe (TypeAnnotatedAST p)
resolveTypes = traverseTypeAnnotations resolveTypeExpr

-- | Resolves a type expression. Since custom type don't exist yet, 
-- | just has a hardcoded list of types
resolveTypeExpr :: TypeExpr -> Maybe KType
resolveTypeExpr (TypeName "Int"   ) = Just KInt
resolveTypeExpr (TypeName "String") = Just KString
resolveTypeExpr (TypeName "Float" ) = Just KFloat
resolveTypeExpr (TypeName "Bool"  ) = Just KBool
resolveTypeExpr (TypeName "Unit"  ) = Just KUnit
resolveTypeExpr (TypeName _       ) = Nothing
resolveTypeExpr (SignatureType argExprs rtExpr) =
    [ KFunc (args $-> rt)
    | args <- traverse resolveTypeExpr argExprs
    , rt   <- resolveTypeExpr rtExpr
    ]
---------------------------------------------------------------------------

--------------------- Adding Type variables ----------------------------------
type MonadTVarSupply m = MonadUniqueSupply TypeVar m
type AnnotatedTVarAST p = AST p 'NoSugar TVarName ('Just KType)

withNewTVar :: MonadTVarSupply m => DesugaredName -> m TVarName
withNewTVar = liftA2 typeAnnotate newTVar . pure

addTVar :: MonadTVarSupply m => TypeAnnotatedAST p -> m (AnnotatedTVarAST p)
addTVar = traverseNames withNewTVar
---------------------------------------------------------------------------

--------------------- Making constraints ----------------------------------
writeProgramConstraints
    :: MonadConstraintGenerator m => AnnotatedTVarAST 'TopLevel -> m ()
writeProgramConstraints (Program funcDefs) = do
    let hoistedCtx = Map.fromList (funcTypeBinding <$> funcDefs)
    withState (<> hoistedCtx) (traverse_ writeFuncDefConstraints funcDefs)
  where
    funcTypeBinding
        :: AnnotatedTVarAST 'FunctionDef -> (DesugaredName, [KType])
    funcTypeBinding (FuncDefAnn name args rt _) =
        (deTypeAnnotate name, [KFunc ((snd <$> args) $-> rt)])

writeFuncDefConstraints
    :: MonadConstraintGenerator m => AnnotatedTVarAST 'FunctionDef -> m ()
writeFuncDefConstraints (FuncDefAnn name args rt body) = do
    funcType <- functionType args rt body
    modify (Map.insertWith (++) (deTypeAnnotate name) [funcType])

stmtReturnTVar
    :: MonadConstraintGenerator m => AnnotatedTVarAST 'Stmt -> m TypeVar
stmtReturnTVar (ExprStmt expr) = exprTVar expr
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
stmtReturnTVar (Var n t expr) =
    checkLocalAssignment n t expr *> pure (TheType KUnit)
stmtReturnTVar (Let n t expr) =
    checkLocalAssignment n t expr *> pure (TheType KUnit)
stmtReturnTVar (Assign name expr) = do
    currentNameTypes <- gets $ Map.lookup (deTypeAnnotate name)
    exprType         <- exprTVar expr
    writeConstraint $ case currentNameTypes of
        Just ts -> IsOneOf exprType ts
        Nothing -> Failure -- The name is unbount, so just fail
    pure $ TheType KUnit

exprTVar
    :: MonadConstraintGenerator m => AnnotatedTVarAST 'Expr -> m TypeVar
exprTVar (LiteralE   l     ) = pure (TheType $ literalType l)
exprTVar (Identifier idName) = do
    -- It must have one of the types that this name has in this scope
    let nameT = nameType idName
    typesInScope <- gets (concat @Maybe . Map.lookup (deTypeAnnotate idName))
    writeConstraint $ IsOneOf nameT typesInScope
    pure nameT

exprTVar (FuncExprAnn args rt body) = TheType <$> functionType args rt body

exprTVar (Call callee args        ) = do
    calleeT <- exprTVar callee
    argT   <- traverse exprTVar args
    let returnT = ApplicationTVar calleeT argT

    writeConstraint $ calleeT =#= FuncTVar (argT $-> returnT)

    pure returnT

functionType
    :: MonadConstraintGenerator m
    => [(TVarName, KType)]
    -> KType
    -> AnnotatedTVarAST 'Stmt
    -> m KType
functionType args rt body = do
    let argCtx = Map.fromList (bimap deTypeAnnotate (pure @[]) <$> args)

    returnTVar <- withState (<> argCtx) (stmtReturnTVar body)
    writeConstraint $ returnTVar =#= TheType rt

    pure . KFunc $ ((snd <$> args) $-> rt)

-- | Assert that the declared type of an assignment matches the actual type
-- | of the expression and add it to the TypeCtx.
checkTopLevelAssignment
    :: MonadConstraintGenerator m
    => TVarName
    -> KType
    -> AnnotatedTVarAST 'Expr
    -> m ()
checkTopLevelAssignment name declaredType expr = do
    exprType <- exprTVar expr
    writeConstraint $ nameType name =#= TheType declaredType
    writeConstraint $ exprType =#= TheType declaredType
    modify (Map.insertWith (++) (deTypeAnnotate name) [declaredType])

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
    gets (Map.lookup (deTypeAnnotate name)) >>= \case
        Just _  -> writeConstraint Failure
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
