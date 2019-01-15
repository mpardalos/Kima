{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.DomainCalculation where

import           Kima.Control.Monad.State.Extended
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Kima.AST
import           Kima.KimaTypes
import           Kima.Typechecking.Types

type MonadDomain m = (MonadState TypeCtx m, MonadError TypecheckingError m)

makeDomains :: TypeCtx -> AnnotatedTVarAST p -> Either TypecheckingError Domains
makeDomains mutTypeCtx ast = evalStateT (calculateDomains ast) mutTypeCtx

makeDomainsWithTypeCtx :: TypeCtx -> AnnotatedTVarAST p -> Either TypecheckingError (Domains, TypeCtx)
makeDomainsWithTypeCtx mutTypeCtx ast = runStateT (calculateDomains ast) mutTypeCtx

calculateDomains :: MonadDomain m => AnnotatedTVarAST p -> m Domains
calculateDomains (Program funcDefs) = do
    let funcTypes = Map.fromList
            (   (\(FuncDefAnn name (fmap snd -> argTypes) rt _) ->
                    (name, [KFunc (argTypes $-> rt)])
                )
            <$> funcDefs
            )
    let hoistedCtx =
            Binding Constant <$> Map.mapKeysWith (<>) deTypeAnnotate funcTypes
    let funcDomains = Map.mapKeysWith (<>) nameType funcTypes
    innerDomains <- mconcat
        <$> withState (<> hoistedCtx) (traverse calculateDomains funcDefs)
    return (innerDomains <> funcDomains)
calculateDomains (FuncDefAnn _name args _ body) = functionDomain args body
calculateDomains (LiteralE _) = pure []
calculateDomains (Identifier name) = lookupIdentifier name >>= \case
    Binding { types } -> pure [(nameType name, types)]
calculateDomains (FuncExprAnn args _ body) = functionDomain args body
calculateDomains (Call callee args       ) = do
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
    (Binding mut ts) <- lookupIdentifier name
    case mut of
        Constant -> throwError (AssignToConst name)
        Variable -> do
            let nameDs = [(nameType name, ts)]
            exprDs <- calculateDomains expr
            return (nameDs <> exprDs)
calculateDomains (Var name t expr) =
    gets (Map.lookup (deTypeAnnotate name)) >>= \case
        Nothing -> do
            modify (Map.insert (deTypeAnnotate name) (Binding Variable [t]) )
            Map.union (Map.singleton (nameType name) [t])
                <$> calculateDomains expr
        Just _ -> throwError (NameShadowed name)
calculateDomains (Let name t expr) =
    gets (Map.lookup (deTypeAnnotate name)) >>= \case
        Nothing -> do
            modify (Map.insert (deTypeAnnotate name) (Binding Constant [t]))
            Map.union (Map.singleton (nameType name) [t])
                <$> calculateDomains expr
        Just _ -> throwError (NameShadowed name)

lookupIdentifier :: MonadDomain m => TVarName -> m Binding
lookupIdentifier name = gets (Map.lookup (deTypeAnnotate name)) >>= \case
    Just b  -> return b
    Nothing -> throwError (UnboundName name)

functionDomain
    :: MonadDomain m
    => [(TVarName, KType)]
    -> AnnotatedTVarAST p
    -> m (Map TypeVar (Set KType))
functionDomain args body =
    let argBindings = Map.fromList (bimap deTypeAnnotate (Binding Constant . Set.singleton) <$> args)
        argDomains  = Map.fromList (bimap nameType Set.singleton <$> args)
    in  (argDomains <>) <$> withState (<> argBindings) (calculateDomains body)
