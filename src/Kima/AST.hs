module Kima.AST where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Kind
import Data.List
import Data.String

import Kima.KimaTypes

---------- Names (identifiers) ---------- 
data BuiltinName = AddOp | SubOp | MulOp | ModOp | DivOp  -- Binary ops
                 | InvertOp | NegateOp -- Unary ops
                 | PrintFunc -- Builtin functions
    deriving (Show, Eq, Ord)

type TypeName        = GenericName 'Nothing                    'False

type ParsedName      = GenericName 'Nothing      'False
type DesugaredName   = GenericName 'Nothing      'True
type TypedName       = GenericName ('Just KType) 'True

data GenericName :: Maybe Type -> Bool -> Type where
    -- Strings
    Name        :: String      -> GenericName 'Nothing b
    TypedName   :: String -> t -> GenericName ('Just t) b

    -- Builtins
    Builtin     :: BuiltinName -> GenericName 'Nothing 'True
    TBuiltin    :: BuiltinName -> t -> GenericName ('Just t) 'True
    
---------- AST ----------  
data Sugar = Sugar | NoSugar
data ASTPart = Expr | Stmt | FunctionDef | TopLevel

data Binary e = Add e e | Sub e e | Div e e | Mul e e | Mod e e 
    deriving (Show, Functor, Foldable, Traversable)

data Unary e = Negate e | Invert e
    deriving (Show, Functor, Foldable, Traversable)

data Literal = IntExpr Integer | FloatExpr Double | BoolExpr Bool | StringExpr String 
    deriving Show

data IfStmt cond body = IfStmt {
    cond :: cond,
    ifBlk :: body,
    elseBlk :: body
}

data WhileStmt cond body = WhileStmt {
    cond :: cond,
    body :: body
}

-- | A Unified AST for all compilation/interpretation phases.
-- | The type parameters specify which constructors are available.
-- | part : whether this is an expression/statement/some other part of the AST
-- | sugar : Whether the AST contains syntactic sugar. 
-- |         e.g. The Desugarer could be desugar :: AST p 'Sugar n s t -> AST p 'NoSugar n s t
-- | name : The type of names (identifiers) in the AST. E.g. A renamer would be:
-- |        rename :: AST ()
-- | typeann : The type of **user-supplied** type annotations. The typecheckr should turn this to **Void**
data AST (part :: ASTPart) (sugar :: Sugar) (name :: Type) (typeAnn :: Maybe Type) where
    Program      :: [AST 'FunctionDef sug n t] -> AST 'TopLevel sug n t

    ----------------------- Top-level function definitions ----------------------- 
    FuncDef      ::  name -> [name]           -> AST 'Stmt sug name 'Nothing -> AST 'FunctionDef sug name 'Nothing
    FuncDefAnn   ::  name -> [(name, t)] -> t -> AST 'Stmt sug name ('Just t)-> AST 'FunctionDef sug name ('Just t)

    ----------------------- Expressions ----------------------- 
    -- Interpreted core
    LiteralE     :: Literal                                                    -> AST 'Expr sug n    t
    Identifier   :: name                                                       -> AST 'Expr sug name t
    FuncExpr     :: [name]           -> AST 'Stmt sug name 'Nothing  -> AST 'Expr sug name 'Nothing
    FuncExprAnn  :: [(name, t)] -> t -> AST 'Stmt sug name ('Just t) -> AST 'Expr sug name ('Just t)
    Call         :: AST 'Expr sug n t -> [AST 'Expr sug n t]        -> AST 'Expr sug n    t
    BinE         :: Binary (AST 'Expr 'Sugar n t)   -> AST 'Expr 'Sugar n t
    UnaryE       :: Unary (AST 'Expr 'Sugar n t)    -> AST 'Expr 'Sugar n t

    ----------------------- Statements  ----------------------- 
    -- Interpreted Core
    ExprStmt :: AST 'Expr sug n t                                 -> AST 'Stmt sug n    t
    Block    :: [AST 'Stmt sug n t]                               -> AST 'Stmt sug n    t
    While    :: WhileStmt (AST 'Expr sug n t) (AST 'Stmt sug n t) -> AST 'Stmt sug n    t
    If       :: IfStmt (AST 'Expr sug n t) (AST 'Stmt sug n t)    -> AST 'Stmt sug n    t
    Assign   :: name -> AST 'Expr sug name t                      -> AST 'Stmt sug name t

    -- Typed versions
    Var      :: name -> t -> AST 'Expr sug name ('Just t) -> AST 'Stmt sug name ('Just t)
    Let      :: name -> t -> AST 'Expr sug name ('Just t) -> AST 'Stmt sug name ('Just t)

------ Type synonyms for different phases -----
-- Parse ->
type ParsedAST    (p :: ASTPart) = AST p 'Sugar   ParsedName    ('Just TypeExpr) -- Desugar ->
type DesugaredAST (p :: ASTPart) = AST p 'NoSugar DesugaredName ('Just TypeExpr) -- Typecheck ->
type TypedAST     (p :: ASTPart) = AST p 'NoSugar TypedName     'Nothing

type ParsedProgram    = ParsedAST 'TopLevel
type DesugaredProgram = DesugaredAST 'TopLevel
type TypedProgram     = TypedAST 'TopLevel

-- Types
data TypeExpr = TypeName (GenericName 'Nothing 'False)
              | SignatureType [TypeExpr] TypeExpr
    deriving Eq

--------- Useful functions ----------
typeAnnotate :: t -> GenericName 'Nothing 'False -> GenericName ('Just t) 'False
typeAnnotate t (Name n) = TypedName n t

--------------- Show instances ---------------------

instance (Show cond, Show stmt) => Show (IfStmt cond stmt) where
    show IfStmt { cond, ifBlk, elseBlk } = "if (" ++ show cond ++ ") " ++ show ifBlk ++ " else " ++ show elseBlk

instance (Show cond, Show stmt) => Show (WhileStmt cond stmt) where
    show WhileStmt { cond, body } = "while (" ++ show cond ++ ") " ++ show body

instance (Show n) => Show (AST p sug n 'Nothing) where
    show (Assign name expr) = show name ++ " = " ++ show expr
    show (BinE bin) = show bin
    show (Block stmts) = "{\n\t" ++ indented (show <$> stmts) ++ "\n}"
        where indented = concat . fmap ("\n\t"++)
    show (Call callee args) = show callee ++ "(" ++ intercalate "," (show <$> args) ++ ")"
    show (ExprStmt expr) = show expr
    show (FuncDef name sig body) = "fun " ++ show name ++ " " ++ show sig ++ " " ++ show body
    show (FuncExpr sig body) = show sig ++ " " ++ show body
    show (Identifier name) = show name
    show (If stmt) = show stmt 
    show (LiteralE lit) = show lit
    show (Program ast) = intercalate "\n" (show <$> ast)
    show (UnaryE unary) = show unary
    show (While stmt) = show stmt 

instance (Show n, Show t) => Show (AST p sug n ('Just t)) where
    show (FuncDefAnn name sig rt body) = "fun " ++ show name ++ " " ++ show sig ++ " -> " ++ show rt ++ show body
    show (FuncExprAnn sig rt body) = show sig ++ " -> " ++ show rt ++ show body
    show (Var name t expr) = "var " ++ show name ++ " : " ++ show t ++ " = " ++ show expr
    show (Let name t expr) = "let " ++ show name ++ " : " ++ show t ++ " = " ++ show expr
    show (Program ast) = intercalate "\n" (show <$> ast)
    show (LiteralE lit) = show lit
    show (Identifier name) = show name
    show (Call callee args) = show callee ++ "(" ++ intercalate "," (show <$> args) ++ ")"
    show (BinE bin) = show bin
    show (UnaryE unary) = show unary
    show (ExprStmt expr) = show expr
    show (Block stmts) = "{\n\t" ++ indented (show <$> stmts) ++ "\n}"
        where indented = concat . fmap ("\n\t"++)
    show (Assign name expr) = show name ++ " = " ++ show expr
    show (While stmt) = show stmt 
    show (If stmt) = show stmt 

instance Show TypeExpr where
    show (TypeName name) = case name of 
        (Name s) -> "#" ++ s
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

-- instance Show ParsedName where
--     show (Name n) = "{" ++ show n ++ "}"


instance Show t => Show (GenericName ('Just t) b) where
    show (TypedName str t) = "{" ++ str ++ " : " ++ show t ++ "}"
    show (TBuiltin n t) = "{" ++ show n ++ " : " ++ show t ++ "}"

instance Show (GenericName 'Nothing b) where
    show (Name n) = "{" ++ show n ++ "}"
    show (Builtin n) = "{" ++ show n ++ "}"

--------------- Boring instances ---------------------

instance Bifunctor IfStmt where
    bimap f g IfStmt {cond, ifBlk, elseBlk} = IfStmt (f cond) (g ifBlk) (g elseBlk)

instance Bifoldable IfStmt where
    bifoldMap f g IfStmt { cond, ifBlk, elseBlk } = f cond <> g ifBlk <> g elseBlk

instance Bitraversable IfStmt where
    bitraverse f g IfStmt { cond, ifBlk, elseBlk } = 
        (\(a, b, c) -> IfStmt a b c) <$> ((,,) <$> f cond <*> g ifBlk <*> g elseBlk)

instance Bifunctor WhileStmt where
    bimap f g WhileStmt {cond, body} = WhileStmt (f cond) (g body) 

instance Bifoldable WhileStmt where
    bifoldMap f g WhileStmt { cond, body } = f cond <> g body

instance Bitraversable WhileStmt where
    bitraverse f g WhileStmt { cond, body } = uncurry WhileStmt <$> bitraverse f g (cond, body)

instance IsString ParsedName where
    fromString = Name

instance Eq ParsedName where
    (Name l) == (Name r) = l == r
instance Eq t => Eq (GenericName ('Just t) b) where
    TypedName n1 t1 == TypedName n2 t2 = n1 == n2 && t1 == t2
    TBuiltin l t1 == TBuiltin r t2 = l == r && t1 == t2
    _ == _ = False

instance Ord ParsedName where
    Name l <= Name r = l <= r
instance Ord t => Ord (GenericName ('Just t) b) where
    TypedName{} `compare` TBuiltin{} = LT
    TBuiltin{} `compare` TypedName{} = GT
    TypedName n1 t1 `compare` TypedName n2 t2 = compare n1 n2 <> compare t1 t2
    TBuiltin l t1 `compare` TBuiltin r t2 = compare l r <> compare t1 t2
