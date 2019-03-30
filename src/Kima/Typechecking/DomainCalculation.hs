{-# LANGUAGE OverloadedLists, AllowAmbiguousTypes #-}
module Kima.Typechecking.DomainCalculation where

import           Kima.Control.Monad.State.Extended
import           Control.Arrow
import           Control.Monad.Except
import qualified Data.Map                      as Map
import           Kima.AST
import           Kima.KimaTypes
import           Kima.Typechecking.Types

type MonadDomain m = (MonadState TypeCtx m, MonadError TypecheckingError m)

makeDomains :: TypeCtx -> TVarAST p -> Either TypecheckingError Domains
makeDomains mutTypeCtx ast = evalStateT (calculateDomains ast) mutTypeCtx

makeDomainsWithTypeCtx :: TypeCtx -> TVarAST p -> Either TypecheckingError (Domains, TypeCtx)
makeDomainsWithTypeCtx mutTypeCtx ast = runStateT (calculateDomains ast) mutTypeCtx

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

calculateDomains :: MonadDomain m => TVarAST p -> m Domains
calculateDomains (Program funcDefs) =
    let hoistedCtx = Map.fromListWith (<>) (funcDefs >>= \case
            FuncDef name args rt _ -> [(Identifier name, Binding Constant [KFunc ((snd <$> args) $-> rt)])]
            DataDef _name _members -> []) in
    mconcat <$> withState
        (addBindings hoistedCtx)
        (traverse calculateDomains funcDefs)
calculateDomains (FuncDef _name args _ body) = functionDomain args body
calculateDomains (DataDef name members) = do
    let declaredType = KUserType name
    let accessorBindings = (\(memberName, memberType) ->
                                ( Accessor memberName
                                , Binding Constant [KFunc ([declaredType] $-> memberType)]))
                           <$> members
    let memberTypes = snd <$> members
    let constructorType = KFunc (memberTypes $-> declaredType)

    modify (addType    name               declaredType)
    modify (addBinding (Identifier name) (Binding Constant [constructorType]))
    modify (addBindings (Map.fromList accessorBindings))
    return []
calculateDomains (LiteralE _) = pure []
calculateDomains (IdentifierE name) = lookupIdentifier name >>= \case
    Binding { types } -> pure [(nameType name, types)]
calculateDomains (FuncExpr args _ body) = functionDomain args body
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
    Binding mut ts <- lookupIdentifier name
    case mut of
        Constant -> throwError (AssignToConst (deTypeAnnotate name))
        Variable -> do
            let nameDs = [(nameType name, ts)]
            exprDs <- calculateDomains expr
            return (nameDs <> exprDs)
calculateDomains (Var name declaredType expr) =
    gets (bindings >>> Map.lookup (Identifier name)) >>= \case
        Nothing -> do
            modify (addBinding (Identifier name) (Binding Variable [declaredType]))
            calculateDomains expr
        Just _ -> throwError (NameShadowed name)
calculateDomains (Let name declaredType expr) =
    gets (bindings >>> Map.lookup (Identifier name)) >>= \case
        Nothing -> do
            modify (addBinding (Identifier name) (Binding Constant [declaredType]))
            calculateDomains expr
        Just _ -> throwError (NameShadowed name)

lookupIdentifier :: MonadDomain m => TVarIdentifier -> m Binding
lookupIdentifier name = gets (bindings >>> Map.lookup (deTypeAnnotate name)) >>= \case
    Just b  -> return b
    Nothing -> throwError (UnboundName name)

functionDomain
    :: MonadDomain m
    => [(Name, KType)]
    -> TVarAST p
    -> m Domains
functionDomain args body =
    let argBindings = Map.fromList (args <&> \(argName, argType) -> (Identifier argName, Binding Constant [argType])) in
    withState (addBindings argBindings) (calculateDomains body)
