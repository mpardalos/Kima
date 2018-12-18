{-# LANGUAGE OverloadedLists #-}
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

-- TODO: Fix confusing Hole/TypeVar terminology
-- TODO: BaseCtx

makeConstraints :: DesugaredAST 'TopLevel -> Maybe (HoleAST 'TopLevel, ConstraintSet)
makeConstraints = 
    resolveTypes                                                 -- Resolve types
        >>> fmap (\annotatedAST -> do
                annotatedHoleAST <- addHoles annotatedAST        -- Add holes 
                addProgramConstraints annotatedHoleAST           -- Generate constraints
                return (removeTypeAnnotations annotatedHoleAST)) -- Remove type annotations
        >>> fmap runConstraintGenerator                          -- Extract everything

-------------- For supply of type variables -------------------
class Monad m => MonadUniqueSupply s m | m -> s where
    -- Law:
    -- pure False == do
    --      a <- supply
    --      b <- supply
    --      pure (a == b)
    supply :: m s

instance MonadUniqueSupply TypeVar ConstraintGenerator where
    supply = ConstraintGenerator . StateT $ \(ctx, hole) -> pure (TypeHole hole, (ctx, hole+1))
---------------------------------------------------------------

-------------- Constraint generation Monad -----------------------------------------------
type MonadConstraintWriter m = MonadWriter ConstraintSet m
type MonadConstraintGenerator m = (MonadConstraintWriter m, MonadState TypeCtx m)

type TypeCtx = Map DesugaredName [KType]

newtype ConstraintGenerator a = ConstraintGenerator (StateT (TypeCtx, Int) (Writer ConstraintSet) a)
    deriving (Functor, Applicative, Monad, MonadWriter ConstraintSet)

instance MonadState TypeCtx ConstraintGenerator where
    get     = ConstraintGenerator . StateT $ \(ctx, hole) -> pure (ctx, (ctx, hole))
    put ctx = ConstraintGenerator . StateT $ \(_, hole)   -> pure ((), (ctx, hole))

runConstraintGenerator :: ConstraintGenerator a -> (a, ConstraintSet)
runConstraintGenerator (ConstraintGenerator cg) =
    cg & (flip evalStateT (mempty, 0) >>> runWriter)

newHole :: MonadHoleSupply m => m TypeVar
addConstraint :: MonadConstraintWriter m => Constraint -> m ()
addConstraints :: MonadConstraintWriter m => ConstraintSet -> m ()
newHole = supply
addConstraint c = tell [c]
addConstraints = tell
-------------------------------------------------------------------------------------------

--------------------- Resolving TypeExprs ----------------------------------
type TypeAnnotatedAST p = AST p 'NoSugar DesugaredName ('Just KType)

resolveTypes :: DesugaredAST p -> Maybe (TypeAnnotatedAST p)
resolveTypes = traverseTypeAnnotations resolveTypeExpr

-- | Resolves a type expression. Since custom type don't exist yet, 
-- | just has a hardcoded list of types
resolveTypeExpr :: TypeExpr -> Maybe KType
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return $ KFunc (args $-> rt)
resolveTypeExpr (TypeName "Int"   ) = Just KInt
resolveTypeExpr (TypeName "String") = Just KString
resolveTypeExpr (TypeName "Float" ) = Just KFloat
resolveTypeExpr (TypeName "Bool"  ) = Just KBool
resolveTypeExpr (TypeName "Unit"  ) = Just KUnit
resolveTypeExpr (TypeName _       ) = Nothing
---------------------------------------------------------------------------

--------------------- Adding Holes ----------------------------------
type MonadHoleSupply m = MonadUniqueSupply TypeVar m
type AnnotatedHoleAST p = AST p 'NoSugar HoleName ('Just KType)

withNewHole :: MonadHoleSupply m => DesugaredName -> m HoleName
withNewHole = liftA2 typeAnnotate newHole . pure

addHoles :: MonadHoleSupply m => TypeAnnotatedAST p -> m (AnnotatedHoleAST p)
addHoles = traverseNames withNewHole
---------------------------------------------------------------------------

--------------------- Making constraints ----------------------------------
addProgramConstraints
    :: MonadConstraintGenerator m => AnnotatedHoleAST 'TopLevel -> m ()
addProgramConstraints (Program ast) = do 
    let hoistedCtx = Map.fromList (funcTypeBinding <$> ast)
    withState (<> hoistedCtx) (traverse_ addFuncDefConstraints ast)
    where
        funcTypeBinding :: AnnotatedHoleAST 'FunctionDef -> (DesugaredName, [KType])
        funcTypeBinding (FuncDefAnn name args rt _) = 
            ( deTypeAnnotate name
            , [KFunc ((snd <$> args) $-> rt)]
            )


addFuncDefConstraints
    :: MonadConstraintGenerator m => AnnotatedHoleAST 'FunctionDef -> m ()
addFuncDefConstraints (FuncDefAnn name args rt body) = do
    funcType <- functionType args rt body
    modify (Map.insertWith (++) (deTypeAnnotate name) [funcType])

stmtReturnTypeVar
    :: MonadConstraintGenerator m => AnnotatedHoleAST 'Stmt -> m TypeVar
stmtReturnTypeVar (ExprStmt expr) = exprTypeVar expr
stmtReturnTypeVar (Block blk) =
    lastDef (TheType KUnit) <$> traverse stmtReturnTypeVar blk
stmtReturnTypeVar (While (WhileStmt cond body)) = do
    condHole  <- exprTypeVar cond
    _bodyHole <- withState id (stmtReturnTypeVar body)

    addConstraint $ condHole =#= TheType KBool

    pure $ TheType KUnit
stmtReturnTypeVar (If (IfStmt cond ifBlk elseBlk)) = do
    condHole    <- exprTypeVar cond
    ifBlkHole   <- withState id (stmtReturnTypeVar ifBlk)
    elseBlkHole <- withState id (stmtReturnTypeVar elseBlk)

    addConstraint $ condHole =#= TheType KBool
    addConstraint $ ifBlkHole =#= elseBlkHole

    pure $ TheType KUnit
stmtReturnTypeVar (Var n t expr) =
    checkLocalAssignment n t expr *> pure (TheType KUnit)
stmtReturnTypeVar (Let n t expr) =
    checkLocalAssignment n t expr *> pure (TheType KUnit)
stmtReturnTypeVar (Assign name expr) = do
    currentNameTypes <- gets $ Map.lookup (deTypeAnnotate name)
    exprType         <- exprTypeVar expr
    addConstraint $ case currentNameTypes of
        Just ts -> IsOneOf exprType ts
        Nothing -> Failure -- The name is unbount, so just fail
    pure $ TheType KUnit

exprTypeVar
    :: MonadConstraintGenerator m => AnnotatedHoleAST 'Expr -> m TypeVar
exprTypeVar (LiteralE   l     ) = pure (TheType $ literalType l)
exprTypeVar (Identifier idName) = do
    -- It must have one of the types that this name has in this scope
    let nameHole = nameType idName
    typesInScope <- gets (concat @Maybe . Map.lookup (deTypeAnnotate idName))
    addConstraint $ IsOneOf nameHole typesInScope
    pure nameHole

exprTypeVar (FuncExprAnn args rt body) = TheType <$> functionType args rt body

exprTypeVar (Call callee args        ) = do
    calleeHole <- exprTypeVar callee
    argHoles   <- traverse exprTypeVar args
    let returnTypeHole = ApplicationHole calleeHole argHoles

    addConstraint $ calleeHole =#= FuncHole (argHoles $-> returnTypeHole)

    pure returnTypeHole

functionType
    :: MonadConstraintGenerator m
    => [(HoleName, KType)]
    -> KType
    -> AnnotatedHoleAST 'Stmt
    -> m KType
functionType args rt body = do
    let argCtx = Map.fromList (bimap deTypeAnnotate (pure @[]) <$> args)

    rtHole <- withState (<> argCtx) (stmtReturnTypeVar body)
    addConstraint $ rtHole =#= TheType rt

    pure . KFunc $ ((snd <$> args) $-> rt)

-- | Assert that the declared type of an assignment matches the actual type
-- | of the expression and add it to the TypeCtx.
checkTopLevelAssignment
    :: MonadConstraintGenerator m
    => HoleName
    -> KType
    -> AnnotatedHoleAST 'Expr
    -> m ()
checkTopLevelAssignment name declaredType expr = do
    exprType <- exprTypeVar expr
    addConstraint $ nameType name =#= TheType declaredType
    addConstraint $ exprType =#= TheType declaredType
    modify (Map.insertWith (++) (deTypeAnnotate name) [declaredType])

-- | Assert that the declared type of an assignment matches the actual type
-- | as well as that the binding does not shadow another name. Shadowing is 
-- | only allowed at the top level.
checkLocalAssignment
    :: MonadConstraintGenerator m
    => HoleName
    -> KType
    -> AnnotatedHoleAST 'Expr
    -> m ()
checkLocalAssignment name declaredType expr = do
    exprType <- exprTypeVar expr
    gets (Map.lookup (deTypeAnnotate name)) >>= \case
        Just _  -> addConstraint Failure
        Nothing -> do
            addConstraint $ nameType name =#= TheType declaredType
            addConstraint $ exprType =#= TheType declaredType
            modify (Map.insert (deTypeAnnotate name) [declaredType])

literalType :: Literal -> KType
literalType IntExpr{}    = KInt
literalType FloatExpr{}  = KFloat
literalType BoolExpr{}   = KBool
literalType StringExpr{} = KString
-------------------------------------- ------------------------------------------------------------------
