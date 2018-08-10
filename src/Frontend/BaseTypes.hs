module Frontend.BaseTypes where

import Data.String
import Data.List

newtype Name = Name String
    deriving (Eq)

data EffectExpr = EffectName Name
data TypeExpr = TypeName Name
              | SignatureType Signature

newtype ArgList = ArgList [(TypeExpr, Name)]
newtype Block = Block [Stmt]

data Signature = Signature {
    arguments :: ArgList,
    returnType :: TypeExpr,
    effect :: EffectExpr
}

data Expr = IntExpr Integer
          | FloatExpr Double
          | BoolExpr Bool
          | FuncExpr Signature Block

data Stmt = FuncStmt Name Signature Block
          | LetStmt Name Expr
          | VarStmt Name Expr
          | ExprStmt Expr
          | WhileStmt Expr Block

-- Basic instances

instance Show Name where
    show (Name str) = "{" ++ str ++ "}"

instance IsString Name where
    fromString = Name
    
instance Show EffectExpr where
    show (EffectName (Name str)) = "[" ++ str ++ "]"

instance Show TypeExpr where
    show (TypeName (Name str)) = "#" ++ str 
    show (SignatureType sig) = "#(" ++ show sig ++ ")"

instance Show Expr where
    show (IntExpr num) = show num
    show (FloatExpr num) = show num
    show (BoolExpr bool) = show bool
    show (FuncExpr sig _) = "func(" ++ show sig ++ ")"

instance Show Stmt where
    show (FuncStmt name sig _) = "func "++ show name ++ "(" ++ show sig ++ ")"
    show (LetStmt name expr  ) = "Let("      ++ show name ++ ", " ++ show expr ++ ")"
    show (VarStmt name expr  ) = "Var("      ++ show name ++ ", " ++ show expr ++ ")"
    show (ExprStmt expr      ) = "ExprStmt(" ++ show expr ++ ")"
    show (WhileStmt cond _   ) = "While("    ++ show cond ++ ")"

instance Show Block where
    show (Block blk) = "{\n" ++ blockShow blk ++ "}"
        where 
            blockShow :: [Stmt] -> String
            blockShow [] = ""
            blockShow (stmt : stmts) = "\t" ++ show stmt ++ "\n" ++ blockShow stmts

instance Show Signature where
    show Signature {arguments, returnType, effect} = 
           show effect
        ++ show arguments
        ++ " -> " ++ show returnType

instance Show ArgList where
    show (ArgList []          ) = ""
    show (ArgList argList) = "(" ++ intercalate ", " (argShow <$> argList) ++ ")"
        where
            argShow :: (TypeExpr, Name) -> String
            argShow (typeName, argName) = show argName ++ " : " ++ show typeName