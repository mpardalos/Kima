module Interpreter.Interpreter where

import           Prelude                 hiding ( lookup )

import           Control.Monad.State.Extended
import           Data.Map
import           Safe

import           AST
import           Interpreter.Types
import           Interpreter.Builtins

eval :: Expr -> Interpreter Value
eval (IntExpr    n        ) = return $ Integer n
eval (FloatExpr  f        ) = return $ Float f
eval (BoolExpr   b        ) = return $ Bool b
eval (StringExpr s        ) = return $ String s
eval (FuncExpr sig body   ) = return $ Function (fst <$> arguments sig) body
eval (IdentifierExpr name ) = getName name
eval (CallExpr callee args) = runFunc <$> eval callee <*> mapM eval args >>= id
eval (BinExpr op l r      ) = evalBinOp op <$> eval l <*> eval r >>= id
eval (UnaryExpr op e      ) = evalUnaryOp op <$> eval e >>= id

runStmt :: Stmt -> Interpreter (Maybe Value)
runStmt (     LetStmt name _t expr) = Nothing <$ bind name <$> eval expr
runStmt (     VarStmt name _t expr) = Nothing <$ bind name <$> eval expr
runStmt (     AssignStmt name expr) = Nothing <$ bind name <$> eval expr
runStmt loop@(WhileStmt  cond body) = eval cond >>= \case
    Bool True  -> runBlock body >> runStmt loop
    Bool False -> return Nothing
    _          -> runtimeError
runStmt (ExprStmt expr) = Just <$> eval expr

runBlock :: Block -> Interpreter Value
runBlock (Block stmts) = do
    res <- lastDef Nothing <$> mapM runStmt stmts
    return $ case res of
        Nothing -> KUnit
        Just v  -> v

runFunc :: Value -> [Value] -> Interpreter Value
runFunc (Function argNames body) args = withState addArgs (runBlock body)
  where
    addArgs :: Environment Value -> Environment Value
    addArgs env = env <> argEnv

    argEnv :: Environment Value
    argEnv = Environment $ fromList (zip argNames args)
runFunc _ _ = runtimeError

getName :: Name -> Interpreter Value
getName name = gets (lookup name . unEnv) >>= \case
    Just val -> return val
    Nothing  -> runtimeError

evalFuncDef :: FuncDef -> Interpreter Value
evalFuncDef FuncDef { signature, body } =
    return $ Function (fst <$> arguments signature) body

bindFuncDef :: FuncDef -> Interpreter ()
bindFuncDef f = bind (name f) =<< evalFuncDef f

runProgram :: Program -> Interpreter ()
runProgram (Program functions) = do
    mapM_ bindFuncDef functions -- Bind all top-level functions
    runFunc <$> getName "main" <*> pure [] >>= const (pure ()) -- Run the main with no arguments
