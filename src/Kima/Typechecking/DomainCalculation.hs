{-# LANGUAGE OverloadedLists, AllowAmbiguousTypes #-}
module Kima.Typechecking.DomainCalculation where

import           Kima.Control.Monad.State.Extended
import           Control.Arrow
import           Control.Monad.Except
import           Data.Foldable
import           Data.List
import qualified Data.Set                      as Set
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
calculateDomains (Program topLevel) = do
    -- Deal with data defs first. Hoists them to the top
    let (dataDefs, notDataDefs) = partition isDataDef topLevel
    traverse_ calculateDomains dataDefs

    -- Then function **declarations**
    let hoistedCtx = Map.fromListWith (<>) (notDataDefs >>= \case
            FuncDef name args rt _ -> [(Identifier name, Binding Constant [KFunc ((snd <$> args) $-> rt)])]
            DataDef _name _members -> [] {- Shouldn't happen -})

    -- Then the actual bodies
    mconcat <$> withState
        (addBindings hoistedCtx)
        (traverse calculateDomains notDataDefs)
        where
            isDataDef DataDef{} = True
            isDataDef _         = False

calculateDomains (FuncDef _name args _ body) = functionDomain args body
calculateDomains (DataDef name members) = do
    let declaredType = KUserType name members
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
calculateDomains (Assign access expr) = assignDomain access expr
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

assignDomain :: forall m. MonadDomain m => WriteAccess TVarName -> TVarAST 'Expr -> m Domains
assignDomain accessor@(WriteAccess (TName baseName baseTVar) targetField) expr =
    lookupIdentifier (TIdentifier baseName baseTVar) >>= \case
        Binding Constant _  -> throwError (AssignToConst accessor)
        Binding Variable baseTypes -> do
            let nameDs = [(baseTVar, baseTypes)]
            subfieldDs <- subfieldDomains (toList baseTypes) targetField
            exprDs     <- calculateDomains expr
            return (nameDs <> exprDs <> subfieldDs)
    where
        subfieldDomains :: [KType]   -- | The possible types of the base of the access
                       -> [TVarName] -- | The sequence of subfields
                       -> m Domains
        subfieldDomains _ []  = return []
        subfieldDomains baseTypes (TName fieldName fieldTVar : subfields) =
            case lookupAll fieldName (fields =<< baseTypes) of
                []            -> throwError (NoSuchField ((\(TName n _) -> n) <$> accessor) baseTypes fieldName)
                possibleTypes -> Map.union [(fieldTVar, Set.fromList possibleTypes)]
                    <$> subfieldDomains possibleTypes subfields

        lookupAll :: Eq a => a -> [(a, b)] -> [b]
        lookupAll x = fmap snd . filter ((==x) . fst)

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
