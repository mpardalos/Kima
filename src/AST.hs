module AST where

import Data.String

newtype Name = Name String
    deriving (Eq, Ord)

data BinOp = Add | Sub | Div | Mul | Mod
    deriving Show

data UnaryOp = Negate | Invert
    deriving Show

data EffectExpr = EffectName Name
    deriving Eq

data TypeExpr = TypeName Name
              | SignatureType [TypeExpr] TypeExpr
    deriving Eq

data NamedSignature = NamedSignature {
         arguments :: [(Name, TypeExpr)],
         returnType :: TypeExpr
     } deriving Eq

newtype Block = Block [Stmt]

newtype Program = Program [FuncDef]

data FuncDef = FuncDef {
    name :: Name,
    signature :: NamedSignature,
    body :: Block
}

data Stmt = LetStmt Name TypeExpr Expr
          | VarStmt Name TypeExpr Expr
          | AssignStmt Name Expr
          | WhileStmt Expr Block
          | ExprStmt Expr

data Expr = IntExpr Integer
          | FloatExpr Double
          | BoolExpr Bool
          | StringExpr String
          | FuncExpr NamedSignature Block
          | CallExpr Expr [Expr]
          | BinExpr BinOp Expr Expr
          | UnaryExpr UnaryOp Expr
          | IdentifierExpr Name

-- Show instances 

instance Show Name where
    show (Name str) = "{" ++ str ++ "}"

instance IsString Name where
    fromString = Name

instance Show FuncDef where
    show FuncDef { name, signature=sig, body } =
        "fun " ++ show name ++ " " ++ show (arguments sig)  ++ " -> " ++ show (returnType sig) ++ " " ++ show body

instance Show EffectExpr where
    show (EffectName (Name str)) = "[" ++ str ++ "]"

instance Show TypeExpr where
    show (TypeName (Name str)) = "#" ++ str
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

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

instance Show NamedSignature where
    show NamedSignature {arguments, returnType} =
           show arguments
        ++ " -> " ++ show returnType