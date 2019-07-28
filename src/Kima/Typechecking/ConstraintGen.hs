{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.ConstraintGen where

import           Control.Arrow                 as Arrow
                                         hiding ( first
                                                , second
                                                )
import           Control.Monad.Writer
import           Data.Foldable

import           Safe

import           Kima.Typechecking.Constraints
import           Kima.AST
import           Kima.KimaTypes

makeConstraints :: AST p TVars -> EqConstraintSet
makeConstraints (ProgramAST ast) = (writeProgramConstraints >>> execWriter) ast
makeConstraints (TopLevelAST ast) = (writeTopLevelConstraints >>> execWriter) ast
makeConstraints (StmtAST    ast) = (stmtReturnTVar >>> execWriter) ast
makeConstraints (ExprAST    ast) = (exprTVar >>> execWriter) ast

--------------------- Making constraints ----------------------------------
type MonadConstraintWriter m = MonadWriter EqConstraintSet m

writeConstraint c = tell [c]

-- | Generate constraints for a top-level AST
writeProgramConstraints :: MonadConstraintWriter m => AST 'Module TVars -> m ()
writeProgramConstraints (Program funcDefs) =
    traverse_ writeTopLevelConstraints funcDefs

-- | Write the constraints for a function definition
writeTopLevelConstraints :: MonadConstraintWriter m => AST 'TopLevel TVars -> m ()
writeTopLevelConstraints (FuncDef _ _ _ body) = void (stmtReturnTVar body)
writeTopLevelConstraints (DataDef _ _members) = pure ()

-- | Compute the return type of a statement and write the constraints required
-- | for typing it
stmtReturnTVar :: MonadConstraintWriter m => AST 'Stmt TVars -> m TypeVar
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
stmtReturnTVar (Var _name declaredType expr) = do
    valueTVar <- exprTVar expr
    writeConstraint $ valueTVar =#= TheType declaredType
    pure (TheType KUnit)
stmtReturnTVar (Let _name declaredType expr) = do
    valueTVar <- exprTVar expr
    writeConstraint $ valueTVar =#= TheType declaredType
    pure (TheType KUnit)
stmtReturnTVar (Assign (WriteAccess (TName _ t) []) expr) = do
    exprType <- exprTVar expr
    writeConstraint (t =#= exprType)
    pure (TheType KUnit)
stmtReturnTVar (Assign (WriteAccess (TName _ nameTVar) subfields) expr) = do
    let lhsTVar = case lastMay subfields of
            Nothing -> nameTVar
            Just (TName _ lastFieldTVar) -> lastFieldTVar
    rhsTVar <- exprTVar expr
    writeConstraint (lhsTVar =#= rhsTVar)
    pure lhsTVar

-- | Compute the type of an expression and write the constraints required for
-- | typing it
exprTVar :: MonadConstraintWriter m => AST 'Expr TVars -> m TypeVar
exprTVar (LiteralE   l            ) = pure (TheType $ literalType l)
exprTVar (IdentifierE idName      ) = pure (nameType idName)
exprTVar (FuncExpr    args rt body) = let argTypes = snd <$> args in
    TheType <$> functionType argTypes rt body
exprTVar (Call callee args        ) = do
    calleeT <- exprTVar callee
    argT    <- traverse exprTVar args
    pure (ApplicationTVar calleeT argT)

functionType :: MonadConstraintWriter m
    => [KType]         -- arg types
    -> KType           -- return type
    -> AST 'Stmt TVars -- body
    -> m KType
functionType argTypes rt body = do
    returnTVar <- stmtReturnTVar body
    writeConstraint $ returnTVar =#= TheType rt

    pure . KFunc $ (argTypes $-> rt)

literalType :: Literal -> KType
literalType IntExpr{}    = KInt
literalType FloatExpr{}  = KFloat
literalType BoolExpr{}   = KBool
literalType StringExpr{} = KString
-------------------------------------- ------------------------------------------------------------------
