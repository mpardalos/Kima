module Kima.Interpreter (
    -- Kima.Interpreter.run,
    -- Kima.Interpreter.runWithEnv,
    E.runModule,
    E.runStmt,
    E.evalExpr,
    E.execInterpreter,
    refify,
    unrefify,
    E.MonadInterpreter,
    E.Environment(..),
    E.Value,
    E.RuntimeError(..)
) where

import Data.IORef.Class

import Kima.Interpreter.Interpreter as E
import Kima.Interpreter.Types as E

-- runAST :: Environment Value -> AST Runtime -> IO (Either RuntimeError Value)
-- runAST env (ModuleAST ast) = do
--     refEnv <- refify env
--     result <- execInterpreter refEnv (E.runProgram ast)
--     return $ Unit <$ result
-- runAST _   (TopLevelAST _  ) = pure (Right Unit)
-- runAST env (StmtAST     ast) = do
--     refEnv <- refify env
--     execInterpreter refEnv (E.runStmt ast)
-- runAST env (ExprAST ast) = do
--     refEnv <- refify env
--     execInterpreter refEnv (E.evalExpr ast)

-- runASTWithEnv
--     :: Environment Value
--     -> AST Runtime
--     -> IO (Either RuntimeError (Value, Environment Value))
-- runASTWithEnv env ast  = do
--     refEnv <- refify env
--     result <- runInterpreter refEnv (runAST ast)
--     mapM (bitraverse pure unrefify) result

-- | Make a new Environment containing references to each value
-- | Equivalent to `traverse newIORef`
refify :: Environment Value -> IO (Environment (IORef Value))
refify = traverse newIORef

-- | Read all references in the environment and make a new Environment
-- | containing the values
-- | Equivalent to `traverse readIORef`
unrefify :: Environment (IORef Value) -> IO (Environment Value)
unrefify = traverse readIORef
