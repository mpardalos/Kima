module Frontend.BaseTypes where

import Data.Void
import Text.Megaparsec

import CommonTypes

type Parser = Parsec Void String

data EffectExpr = EffectName Name
    deriving Eq

data TypeExpr = TypeName Name
              | SignatureType Signature
    deriving Eq

data Signature = Signature {
    arguments :: [TypeExpr],
    returnType :: TypeExpr
} deriving Eq

newtype Program = Program [FuncDef]

data FuncDef = FuncDef {
    name :: Name,
    args :: [(TypeExpr, Name)],
    returnType :: TypeExpr,
    body :: Block
}

newtype Block = Block [Stmt]
data Stmt = LetStmt Name TypeExpr Expr
          | VarStmt Name TypeExpr Expr
          | AssignStmt Name Expr
          | WhileStmt Expr Block
          | ExprStmt Expr

data Expr = IntExpr Integer
          | FloatExpr Double
          | BoolExpr Bool
          | StringExpr String
          | FuncExpr Signature Block
          | CallExpr Expr [Expr]
          | BinExpr BinOp Expr Expr
          | UnaryExpr UnaryOp Expr
          | IdentifierExpr Name

-- Basic instances

instance Show FuncDef where
    show FuncDef { name, args, returnType, body } = 
        "fun " ++ show name ++ " " ++ show args  ++ " -> " ++ show returnType ++ " " ++ show body

instance Show EffectExpr where
    show (EffectName (Name str)) = "[" ++ str ++ "]"

instance Show TypeExpr where
    show (TypeName (Name str)) = "#" ++ str 
    show (SignatureType sig) = "#(" ++ show sig ++ ")"

instance Show Expr where
    show (IntExpr num) = show num
    show (FloatExpr num) = show num
    show (BoolExpr bool) = show bool
    show (StringExpr str) = show str
    show (FuncExpr sig _) = "func(" ++ show sig ++ ")"
    show (CallExpr funcExpr arg) = "app(" ++ show funcExpr ++ ";" ++ show arg ++ ")"
    show (BinExpr op l r) = show op ++ "(" ++ show l ++ ";" ++ show r ++ ")"
    show (UnaryExpr op e) = show op ++ "(" ++ show e ++ ")"
    show (IdentifierExpr name) = show name

instance Show Stmt where
    show (LetStmt    name t expr) = "Let("      ++ show name ++ ": " ++ show t ++ " = " ++ show expr ++ ")"
    show (VarStmt    name t expr) = "Var("      ++ show name ++ ": " ++ show t ++ " = " ++ show expr ++ ")"
    show (AssignStmt name   expr) = "Assign("   ++ show name ++ ", " ++ show expr ++ ")"
    show (ExprStmt          expr) = "ExprStmt(" ++ show expr ++ ")"
    show (WhileStmt       cond _) = "While("    ++ show cond ++ ")"

instance Show Block where
    show (Block blk) = "{\n" ++ blockShow blk ++ "}"
        where 
            blockShow :: [Stmt] -> String
            blockShow [] = ""
            blockShow (stmt : stmts) = "\t" ++ show stmt ++ "\n" ++ blockShow stmts

instance Show Signature where
    show Signature {arguments, returnType} = 
           show arguments
        ++ " -> " ++ show returnType

-- instance Show ArgList where
--     show (ArgList []          ) = ""
--     show (ArgList argList) = "(" ++ L.intercalate ", " (argShow <$> argList) ++ ")"
--         where
--             argShow :: (TypeExpr, Name) -> String
--             argShow (typeName, argName) = show argName ++ " : " ++ show typeName