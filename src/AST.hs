module AST where

import Data.String
import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show()

newtype Name = Name String
    deriving (Eq, Ord)

data EffectExpr = EffectName Name

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

-- Expressions
type Expr = Term ExprF

type ExprF = (BinExpr :+: UnaryExpr :+: Literal :+: Identifier :+: FuncExpr :+: Call)
data BinExpr e       = Add e e 
                     | Sub e e 
                     | Div e e 
                     | Mul e e 
                     | Mod e e
                     deriving Functor
data UnaryExpr e     = Negate e 
                     | Invert e
                     deriving Functor
data Literal e       = IntExpr Integer
                     | FloatExpr Double
                     | BoolExpr Bool
                     | StringExpr String 
                     deriving Functor
newtype Identifier e = IdentifierExpr Name
                     deriving Functor
data FuncExpr e      = FuncExpr NamedSignature Block
                     deriving Functor
data Call e          = CallExpr e [e]
                     deriving Functor

$(derive [makeShowF, makeFoldable, makeTraversable, smartAConstructors, smartConstructors]
 [''BinExpr, ''UnaryExpr, ''Literal, ''Identifier, ''FuncExpr, ''Call])

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