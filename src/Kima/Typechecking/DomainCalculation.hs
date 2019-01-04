{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.DomainCalculation where

import           Kima.Control.Monad.State.Extended
import           Control.Monad.Except
import           Control.Applicative
import           Data.Bifunctor
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Kima.AST
import           Kima.KimaTypes
import           Kima.Typechecking.Types
import           Kima.Builtins

type MonadDomain m = (MonadState TypeCtx m, MonadError TypecheckingError m)

makeDomains :: AnnotatedTVarAST p -> Either TypecheckingError Domains
makeDomains ast = evalStateT (calculateDomains ast) baseTypeCtx

calculateDomains :: MonadDomain m => AnnotatedTVarAST p -> m Domains
calculateDomains (Program funcDefs) = do
    let funcTypes = Map.fromList
            (   (\(FuncDefAnn name (fmap snd -> argTypes) rt _) ->
                    (name, Set.singleton (KFunc (argTypes $-> rt)))
                )
            <$> funcDefs
            )
    let hoistedCtx  = Map.mapKeysWith (<>) deTypeAnnotate funcTypes
    let funcDomains = Map.mapKeysWith (<>) nameType funcTypes
    innerDomains <- mconcat
        <$> withState (<> hoistedCtx) (traverse calculateDomains funcDefs)
    return (innerDomains <> funcDomains)
calculateDomains (FuncDefAnn _name args _ body) = functionDomain args body
calculateDomains (LiteralE   _                ) = pure []
calculateDomains (Identifier name             ) = identifierDomain name
calculateDomains (FuncExprAnn args _ body     ) = functionDomain args body
calculateDomains (Call callee args            ) = do
    calleeDs <- calculateDomains callee
    argDs    <- mconcat <$> traverse calculateDomains args
    return (calleeDs <> argDs)
calculateDomains (ExprStmt expr) = calculateDomains expr
calculateDomains (Block stmts) = mconcat <$> traverse calculateDomains stmts
calculateDomains (While (WhileStmt cond stmt)) = do
    condDs <- calculateDomains cond
    bodyDs <- calculateDomains stmt
    return (condDs <> bodyDs)
calculateDomains (If (IfStmt cond ifBlk elseBlk)) = do
    condDs    <- calculateDomains cond
    ifBlkDs   <- calculateDomains ifBlk
    elseBlkDs <- calculateDomains elseBlk
    return (condDs <> ifBlkDs <> elseBlkDs)
calculateDomains (Assign name expr) = do
    nameDs <- identifierDomain name
    exprDs <- calculateDomains expr
    return (nameDs <> exprDs)
calculateDomains (Var name t expr) = do
    modify (Map.insert (deTypeAnnotate name) [t])
    Map.union (Map.singleton (nameType name) [t]) <$> calculateDomains expr
calculateDomains (Let name t expr) = do
    modify (Map.insert (deTypeAnnotate name) [t])
    Map.union (Map.singleton (nameType name) [t]) <$> calculateDomains expr

identifierDomain :: MonadDomain m => TVarName -> m Domains
identifierDomain name = gets (Map.lookup (deTypeAnnotate name)) >>= \case
    Just ts -> pure (Map.singleton (nameType name) ts)
    Nothing -> throwError (UnboundName name)

-- functionDomain :: MonadDomain m =>  m (Map TypeVar (Set KType))
functionDomain
    :: MonadDomain m
    => [(TVarName, KType)]
    -> AnnotatedTVarAST p
    -> m (Map TypeVar (Set KType))
functionDomain args body =
    let argCtx     = Map.fromList (bimap deTypeAnnotate Set.singleton <$> args)
        argDomains = Map.fromList (bimap nameType Set.singleton <$> args)
    in  liftA2 (<>)
               (pure argDomains)
               (withState (<> argCtx) (calculateDomains body))
