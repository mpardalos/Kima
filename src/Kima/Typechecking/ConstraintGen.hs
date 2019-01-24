{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.ConstraintGen where

import           Control.Arrow                 as Arrow
                                         hiding ( first
                                                , second
                                                )
import           Control.Monad.Writer
import           Data.Foldable

import           Safe

import           Kima.Typechecking.Types
import           Kima.AST
import           Kima.KimaTypes

makeConstraints :: AnnotatedTVarAST p -> EqConstraintSet
makeConstraints (ProgramAST ast) = (writeProgramConstraints >>> execWriter) ast
makeConstraints (TopLevelAST ast) = (writeTopLevelConstraints >>> execWriter) ast
makeConstraints (StmtAST    ast) = (stmtReturnTVar >>> execWriter) ast
makeConstraints (ExprAST    ast) = (exprTVar >>> execWriter) ast

--------------------- Making constraints ----------------------------------
type MonadConstraintWriter m = MonadWriter EqConstraintSet m

writeConstraint c = tell [c]

-- | Generate constraints for a top-level AST
writeProgramConstraints
    :: MonadConstraintWriter m => AnnotatedTVarAST 'Module -> m ()
writeProgramConstraints (Program funcDefs) =
    traverse_ writeTopLevelConstraints funcDefs

-- | Write the constraints for a function definition
writeTopLevelConstraints
    :: MonadConstraintWriter m => AnnotatedTVarAST 'TopLevel -> m ()
writeTopLevelConstraints (FuncDefAnn _ _ _ body) = void (stmtReturnTVar body)
writeTopLevelConstraints (DataDefAnn _ _members) = pure ()

-- | Compute the return type of a statement and write the constraints required
-- | for typing it
stmtReturnTVar
    :: MonadConstraintWriter m => AnnotatedTVarAST 'Stmt -> m TypeVar
stmtReturnTVar (ExprStmt expr) = do
    tvar <- exprTVar expr
    writeConstraint (tvar =#= tvar)
    return tvar
stmtReturnTVar (Block blk) =
    lastDef (TheType KUnit) <$> traverse stmtReturnTVar blk
stmtReturnTVar (While (WhileStmt cond body)) = do
    condT  <- exprTVar cond
    _bodyT <- stmtReturnTVar body

    writeConstraint $ condT =#= TheType KBool

    pure (TheType KUnit)
stmtReturnTVar (If (IfStmt cond ifBlk elseBlk)) = do
    condT    <- exprTVar cond
    ifBlkT   <- stmtReturnTVar ifBlk
    elseBlkT <- stmtReturnTVar elseBlk

    writeConstraint $ condT =#= TheType KBool
    writeConstraint $ ifBlkT =#= elseBlkT

    pure (TheType KUnit)
stmtReturnTVar (Var name declaredTyped expr) = do
    checkLocalAssignment name declaredTyped expr
    pure (TheType KUnit)
stmtReturnTVar (Let name t expr) = do
    checkLocalAssignment name t expr
    pure (TheType KUnit)
stmtReturnTVar (Assign name expr) = do
    exprType <- exprTVar expr
    writeConstraint (nameType name =#= exprType)
    pure (TheType KUnit)

-- | Compute the type of an expression and write the constraints required for
-- | typing it
exprTVar :: MonadConstraintWriter m => AnnotatedTVarAST 'Expr -> m TypeVar
exprTVar (LiteralE   l            ) = pure (TheType $ literalType l)
exprTVar (Identifier idName       ) = pure (nameType idName)
exprTVar (FuncExprAnn args rt body) = TheType <$> functionType args rt body
exprTVar (Call callee args        ) = do
    calleeT <- exprTVar callee
    argT    <- traverse exprTVar args
    pure (ApplicationTVar calleeT argT)

functionType
    :: MonadConstraintWriter m
    => [(TVarName, KType)]
    -> KType
    -> AnnotatedTVarAST 'Stmt
    -> m KType
functionType args rt body = do
    returnTVar <- stmtReturnTVar body
    writeConstraint $ returnTVar =#= TheType rt

    pure . KFunc $ ((snd <$> args) $-> rt)

-- | Assert that the declared type of an assignment matches the actual type
-- | as well as that the binding does not shadow another name. Shadowing is 
-- | only allowed at the top level.
checkLocalAssignment
    :: MonadConstraintWriter m
    => TVarName
    -> KType
    -> AnnotatedTVarAST 'Expr
    -> m ()
checkLocalAssignment name declaredType expr = do
    exprType <- exprTVar expr
    writeConstraint $ nameType name =#= TheType declaredType
    writeConstraint $ exprType =#= TheType declaredType

literalType :: Literal -> KType
literalType IntExpr{}    = KInt
literalType FloatExpr{}  = KFloat
literalType BoolExpr{}   = KBool
literalType StringExpr{} = KString
-------------------------------------- ------------------------------------------------------------------
