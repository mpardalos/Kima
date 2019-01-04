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

data Mutability = Constant | Variable
    deriving (Eq, Ord, Show)
instance Semigroup Mutability where
    Constant <> _ = Constant
    _ <> Constant = Constant
    _ <> _        = Variable
type MutTypeCtx = Map DesugaredName (Mutability, Set KType)

type MonadDomain m = (MonadState MutTypeCtx m, MonadError TypecheckingError m)

makeDomains :: AnnotatedTVarAST p -> Either TypecheckingError Domains
makeDomains ast = evalStateT (calculateDomains ast) baseMutTypeCtx
  where
    baseMutTypeCtx :: MutTypeCtx
    baseMutTypeCtx = (,) Constant <$> baseTypeCtx

calculateDomains :: MonadDomain m => AnnotatedTVarAST p -> m Domains
calculateDomains (Program funcDefs) = do
    let funcTypes = Map.fromList
            (   (\(FuncDefAnn name (fmap snd -> argTypes) rt _) ->
                    (name, Set.singleton (KFunc (argTypes $-> rt)))
                )
            <$> funcDefs
            )
    let hoistedCtx =
            (,) Constant <$> Map.mapKeysWith (<>) deTypeAnnotate funcTypes
    let funcDomains = Map.mapKeysWith (<>) nameType funcTypes
    innerDomains <- mconcat
        <$> withState (<> hoistedCtx) (traverse calculateDomains funcDefs)
    return (innerDomains <> funcDomains)
calculateDomains (FuncDefAnn _name args _ body) = functionDomain args body
calculateDomains (LiteralE _) = pure []
calculateDomains (Identifier name) = lookupIdentifier name >>= \case
    (_mut, ts) -> pure [(nameType name, ts)]
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
    (mut, ts) <- lookupIdentifier name
    case mut of
        Constant -> throwError (AssignToConst name)
        Variable -> do
            let nameDs = [(nameType name, ts)]
            exprDs <- calculateDomains expr
            return (nameDs <> exprDs)
calculateDomains (Var name t expr) =
    gets (Map.lookup (deTypeAnnotate name)) >>= \case
        Nothing -> do
            modify (Map.insert (deTypeAnnotate name) (Variable, [t]))
            Map.union (Map.singleton (nameType name) [t])
                <$> calculateDomains expr
        Just _ -> throwError (NameShadowed name)
calculateDomains (Let name t expr) =
    gets (Map.lookup (deTypeAnnotate name)) >>= \case
        Nothing -> do
            modify (Map.insert (deTypeAnnotate name) (Constant, [t]))
            Map.union (Map.singleton (nameType name) [t])
                <$> calculateDomains expr
        Just _ -> throwError (NameShadowed name)

lookupIdentifier :: MonadDomain m => TVarName -> m (Mutability, Set KType)
lookupIdentifier name = gets (Map.lookup (deTypeAnnotate name)) >>= \case
    Just (mut, ts) -> pure (mut, ts)
    Nothing        -> throwError (UnboundName name)

-- functionDomain :: MonadDomain m =>  m (Map TypeVar (Set KType))
functionDomain
    :: MonadDomain m
    => [(TVarName, KType)]
    -> AnnotatedTVarAST p
    -> m (Map TypeVar (Set KType))
functionDomain args body =
    let argCtx =
            Map.fromList
                (bimap deTypeAnnotate ((,) Constant . Set.singleton) <$> args)
        argDomains = Map.fromList (bimap nameType Set.singleton <$> args)
    in  liftA2 (<>)
               (pure argDomains)
               (withState (<> argCtx) (calculateDomains body))
