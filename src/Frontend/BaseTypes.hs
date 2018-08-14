module Frontend.BaseTypes where

import Data.List

import CommonTypes

data EffectExpr = EffectName Name
    deriving Eq
data TypeExpr = TypeName Name
              | SignatureType Signature
    deriving Eq

newtype ArgList = ArgList {unArgList :: [(TypeExpr, Name)]}
    deriving Eq

data Signature = Signature {
    arguments :: ArgList,
    returnType :: TypeExpr,
    effect :: EffectExpr
} deriving Eq

newtype Program = Program [FuncDef]
data FuncDef = FuncDef {
    signature :: Signature,
    body :: Block
}

newtype Block = Block [Stmt]
data Stmt = LetStmt Name TypeExpr Expr
          | VarStmt Name TypeExpr Expr
          | WhileStmt Expr Block
          | ExprStmt Expr

data Expr = IntExpr Integer
          | FloatExpr Double
          | BoolExpr Bool
          | FuncExpr Signature Block
          | CallExpr Expr [Expr]
          | BinExpr BinOp Expr Expr
          | IdentifierExpr Name

-- Basic instances

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
    show (CallExpr funcExpr arg) = "app(" ++ show funcExpr ++ ";" ++ show arg ++ ")"
    show (BinExpr op l r) = show op ++ "(" ++ show l ++ ";" ++ show r ++ ")"
    show (IdentifierExpr name) = show name

instance Show Stmt where
    show (LetStmt name _ expr) = "Let("      ++ show name ++ ", " ++ show expr ++ ")"
    show (VarStmt name _ expr) = "Var("      ++ show name ++ ", " ++ show expr ++ ")"
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