module Kima.AST where

import Control.Applicative
import Control.Monad.Identity
import Data.Coerce
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Kind
import Data.List
import Data.String

import GHC.Generics

import Kima.KimaTypes

---------- Names (identifiers) ---------- 
data BuiltinName = AddOp | SubOp | MulOp | ModOp | DivOp  -- Binary ops
                 | GTOp | GTEOp | LTOp | LTEOp | EqualsOp
                 | InvertOp | NegateOp -- Unary ops
                 | PrintFunc | InputFunc -- Builtin functions
    deriving (Show, Eq, Ord, Generic)

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
              | Less e e | LessEq e e | Greater e e | GreatEq e e | Eq e e | NotEq e e
    deriving (Show, Functor, Foldable, Traversable, Generic)

data Unary e = Negate e | Invert e
    deriving (Show, Functor, Foldable, Traversable, Generic)

data Literal = IntExpr Integer | FloatExpr Double | BoolExpr Bool | StringExpr String 
    deriving (Generic)

data IfStmt cond body = IfStmt {
    cond :: cond,
    ifBlk :: body,
    elseBlk :: body
}

data WhileStmt cond body = WhileStmt {
    cond :: cond,
    body :: body
}

newtype ArgList a = ArgList [a]

-- | A Unified AST for all compilation/interpretation phases.
-- | The type parameters specify which constructors are available.
-- | part : whether this is an expression/statement/some other part of the AST
-- | sugar : Whether the AST contains syntactic sugar. 
-- |         e.g. The Desugarer could be desugar :: AST p 'Sugar n s t -> AST p 'NoSugar n s t
-- | name : The type of names (identifiers) in the AST. E.g. A renamer would be:
-- |        rename :: AST ()
-- | typeann : The type of **user-supplied** type annotations. The typechecker should turn this to **Void**
data AST (part :: ASTPart) (sugar :: Sugar) (name :: Type) (typeAnn :: Maybe Type) where
    Program      :: [AST 'FunctionDef sug n t] -> AST 'TopLevel sug n t

    ----------------------- Top-level function definitions ----------------------- 
    FuncDef      ::  name -> [name] -> AST 'Stmt sug name 'Nothing -> AST 'FunctionDef sug name 'Nothing
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
typeAnnotate :: t -> GenericName a b -> GenericName ('Just t) b
typeAnnotate t (Name n) = TypedName n t
typeAnnotate t (Builtin n) = TBuiltin n t
typeAnnotate t (TypedName n _) = TypedName n t
typeAnnotate t (TBuiltin n _) = TBuiltin n t

deTypeAnnotate :: GenericName ('Just t) b -> GenericName 'Nothing b
deTypeAnnotate (TypedName n _ ) = Name n
deTypeAnnotate (TBuiltin  n _ ) = Builtin n

nameType :: GenericName ('Just t) b -> t
nameType (TypedName _ t) = t
nameType (TBuiltin _ t) = t

--------------- Show instances ---------------------

instance (Show cond, Show stmt) => Show (IfStmt cond stmt) where
    show IfStmt { cond, ifBlk, elseBlk } = "if (" ++ show cond ++ ") " ++ show ifBlk ++ " else " ++ show elseBlk

instance (Show cond, Show stmt) => Show (WhileStmt cond stmt) where
    show WhileStmt { cond, body } = "while (" ++ show cond ++ ") " ++ show body

instance Show a => Show (ArgList a) where
    show (ArgList args) = "(" <> intercalate ", " (show <$> args) <> ")"

instance Show Literal where
    show (IntExpr n    ) = "i" <> show n
    show (FloatExpr f  ) = "f" <> show f
    show (BoolExpr b   ) = show b
    show (StringExpr s ) = "s" <> show s

instance (Show n) => Show (AST p sug n 'Nothing) where
    show (Assign name expr) = show name ++ " = " ++ show expr
    show (BinE bin) = show bin
    show (Block stmts) = "{\n\t" ++ indented (show <$> stmts) ++ "\n}"
        where indented = concatMap ("\n\t"++)
    show (Call callee args) = show callee ++ "(" ++ intercalate "," (show <$> args) ++ ")"
    show (ExprStmt expr) = show expr
    show (FuncDef name sig body) = "fun " ++ show name ++ " " ++ show (ArgList sig) ++ " " ++ show body
    show (FuncExpr sig body) = show (ArgList sig) ++ " " ++ show body
    show (Identifier name) = show name
    show (If stmt) = show stmt 
    show (LiteralE lit) = show lit
    show (Program ast) = intercalate "\n" (show <$> ast)
    show (UnaryE unary) = show unary
    show (While stmt) = show stmt 

instance (Show n, Show t) => Show (AST p sug n ('Just t)) where
    show (FuncDefAnn name sig rt body) = "fun " ++ show name ++ " " ++ show (ArgList sig) ++ " -> " ++ show rt ++ show body
    show (FuncExprAnn sig rt body@Block{}) 
        = "fun " ++ show (ArgList sig) ++ " -> " ++ show rt ++ show body
    show (FuncExprAnn sig rt body) 
        = "fun " ++ show (ArgList sig) ++ " -> " ++ show rt ++ show (Block [body])
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
        where indented = concatMap ("\n\t"++)
    show (Assign name expr) = show name ++ " = " ++ show expr
    show (While stmt) = show stmt 
    show (If stmt) = show stmt 

instance Show TypeExpr where
    show (TypeName (Name s)) = "#\"" ++ s ++ "\""
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

instance Show t => Show (GenericName ('Just t) b) where
    show (TypedName str t) = "{" ++ str ++ " : " ++ show t ++ "}"
    show (TBuiltin n t) = "{" ++ show n ++ " : " ++ show t ++ "}"

instance Show (GenericName 'Nothing b) where
    show (Name str) = "{" ++ str ++ "}"
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

instance Eq (GenericName 'Nothing b) where
    (Name l) == (Name r) = l == r
    (Builtin l) == (Builtin r) = l == r
    _ == _ = False
instance Eq t => Eq (GenericName ('Just t) b) where
    TypedName n1 t1 == TypedName n2 t2 = n1 == n2 && t1 == t2
    TBuiltin l t1 == TBuiltin r t2 = l == r && t1 == t2
    _ == _ = False

instance Ord (GenericName 'Nothing b) where
    Name{} `compare` Builtin{} = LT
    Builtin{} `compare` Name{} = GT
    Name n1 `compare` Name n2 = compare n1 n2
    Builtin l `compare` Builtin r = compare l r

instance Ord t => Ord (GenericName ('Just t) b) where
    TypedName{} `compare` TBuiltin{} = LT
    TBuiltin{} `compare` TypedName{} = GT
    TypedName n1 t1 `compare` TypedName n2 t2 = compare n1 n2 <> compare t1 t2
    TBuiltin l t1 `compare` TBuiltin r t2 = compare l r <> compare t1 t2

-- Traverals

mapTypeAnnotations :: forall n t1 t2 p sug. (t1 -> t2) -> AST p sug n ('Just t1) -> AST p sug n ('Just t2)
mapTypeAnnotations = coerce (traverseTypeAnnotations :: (t1 -> Identity t2) -> AST p sug n ('Just t1) -> Identity (AST p sug n ('Just t2)))

traverseTypeAnnotations :: Applicative m => (t1 -> m t2) -> AST p sug n ('Just t1) -> m (AST p sug n ('Just t2))
traverseTypeAnnotations f (Program ast)            = Program <$> traverse (traverseTypeAnnotations f) ast
traverseTypeAnnotations f (FuncDefAnn n args rt b) = FuncDefAnn n <$> traverse (traverse f) args <*> f rt <*> traverseTypeAnnotations f b
traverseTypeAnnotations _ (LiteralE lit)           = pure $ LiteralE lit
traverseTypeAnnotations _ (Identifier n)           = pure $ Identifier n
traverseTypeAnnotations f (FuncExprAnn args rt b)  = FuncExprAnn <$> traverse (traverse f) args       <*> f rt <*> traverseTypeAnnotations f b
traverseTypeAnnotations f (Call callee args)       = Call        <$> traverseTypeAnnotations f callee <*> traverse (traverseTypeAnnotations f) args
traverseTypeAnnotations f (BinE bin)               = BinE        <$> traverse (traverseTypeAnnotations f) bin
traverseTypeAnnotations f (UnaryE unary)           = UnaryE      <$> traverse (traverseTypeAnnotations f) unary
traverseTypeAnnotations f (ExprStmt e)             = ExprStmt    <$> traverseTypeAnnotations f e
traverseTypeAnnotations f (Block blk)              = Block       <$> traverse (traverseTypeAnnotations f) blk
traverseTypeAnnotations f (While stmt)             = While       <$> bitraverse (traverseTypeAnnotations f) (traverseTypeAnnotations f) stmt
traverseTypeAnnotations f (If stmt)                = If          <$> bitraverse (traverseTypeAnnotations f) (traverseTypeAnnotations f) stmt
traverseTypeAnnotations f (Assign n e)             = Assign n    <$> traverseTypeAnnotations f e
traverseTypeAnnotations f (Var n t e)              = Var n       <$> f t <*> traverseTypeAnnotations f e
traverseTypeAnnotations f (Let n t e)              = Let n       <$> f t <*> traverseTypeAnnotations f e

removeTypeAnnotations :: AST p sug n ('Just t) -> AST p sug n 'Nothing 
removeTypeAnnotations (Program ast)           = Program (removeTypeAnnotations <$> ast) 
removeTypeAnnotations (FuncDefAnn n args _ b) = FuncDef n (fst <$> args) (removeTypeAnnotations b)
removeTypeAnnotations (LiteralE lit)          = LiteralE lit
removeTypeAnnotations (Identifier n)          = Identifier n
removeTypeAnnotations (FuncExprAnn args _ b)  = FuncExpr (fst <$> args) (removeTypeAnnotations b)
removeTypeAnnotations (Call callee args)      = Call (removeTypeAnnotations callee) (removeTypeAnnotations <$> args)
removeTypeAnnotations (BinE bin)              = BinE (removeTypeAnnotations <$> bin) 
removeTypeAnnotations (UnaryE unary)          = UnaryE (removeTypeAnnotations <$> unary)
removeTypeAnnotations (ExprStmt e)            = ExprStmt (removeTypeAnnotations e)
removeTypeAnnotations (Block blk)             = Block (removeTypeAnnotations <$> blk)
removeTypeAnnotations (While stmt)            = While (bimap removeTypeAnnotations removeTypeAnnotations stmt)
removeTypeAnnotations (If stmt)               = If (bimap removeTypeAnnotations removeTypeAnnotations stmt)
removeTypeAnnotations (Assign n e)            = Assign n (removeTypeAnnotations e)
removeTypeAnnotations (Var n _ e)             = Assign n (removeTypeAnnotations e)
removeTypeAnnotations (Let n _ e)             = Assign n (removeTypeAnnotations e)

mapNames :: forall n1 n2 p sug t. (n1 -> n2) -> AST p sug n1 t -> AST p sug n2 t
mapNames = coerce (traverseNames :: (n1 -> Identity n2) -> AST p sug n1 t -> Identity (AST p sug n2 t))

traverseNames :: Applicative m => (n1 -> m n2) -> AST p sug n1 t -> m (AST p sug n2 t)
traverseNames f (Program ast)            = Program <$> traverse (traverseNames f) ast
traverseNames f (ExprStmt e)             = ExprStmt <$> traverseNames f e
traverseNames _ (LiteralE lit)           = pure $ LiteralE lit
traverseNames f (Identifier n)           = Identifier <$> f n
traverseNames f (BinE bin)               = BinE       <$> traverse (traverseNames f) bin
traverseNames f (UnaryE unary)           = UnaryE     <$> traverse (traverseNames f) unary
traverseNames f (While stmt)             = While <$> bitraverse (traverseNames f) (traverseNames f) stmt
traverseNames f (If stmt)                = If    <$> bitraverse (traverseNames f) (traverseNames f) stmt
traverseNames f (Block blk)              = Block <$> traverse   (traverseNames f)                   blk
traverseNames f (FuncDef n args b)       = FuncDef     <$> f n <*> traverse f args                              <*> traverseNames f b
traverseNames f (FuncExpr    args    b)  = FuncExpr    <$>         traverse f args                              <*> traverseNames f b
traverseNames f (FuncDefAnn n args rt b) = FuncDefAnn  <$> f n <*> traverse (traverseTuple1 f) args <*> pure rt <*> traverseNames f b
traverseNames f (FuncExprAnn args rt b)  = FuncExprAnn <$>         traverse (traverseTuple1 f) args <*> pure rt <*> traverseNames f b
traverseNames f (Call callee args)       = Call        <$> traverseNames f callee                               <*> traverse (traverseNames f) args
traverseNames f (Assign n e)             = Assign <$> f n            <*> traverseNames f e
traverseNames f (Var n t e)              = Var    <$> f n <*> pure t <*> traverseNames f e
traverseNames f (Let n t e)              = Let    <$> f n <*> pure t <*> traverseNames f e

traverseTuple1 :: Applicative m => (a -> m c) -> (a, b) -> m (c, b)
traverseTuple1 f (a, b) = liftA2 (,) (f a) (pure b)

-- Patterns
{-# COMPLETE StmtAST, ExprAST, ProgramAST, FuncDefAST#-}

pattern StmtAST :: AST 'Stmt s n t -> AST p s n t
pattern StmtAST stmt <- (isStmt -> Just stmt)

isStmt :: AST p s n t -> Maybe (AST 'Stmt s n t)
isStmt BinE{}          = Nothing
isStmt Call{}          = Nothing
isStmt FuncDef{}       = Nothing
isStmt FuncDefAnn{}    = Nothing
isStmt FuncExpr{}      = Nothing
isStmt FuncExprAnn{}   = Nothing
isStmt Identifier{}    = Nothing
isStmt LiteralE{}      = Nothing
isStmt Program{}       = Nothing
isStmt stmt@Assign{}   = Just stmt
isStmt stmt@Block{}    = Just stmt
isStmt stmt@ExprStmt{} = Just stmt
isStmt stmt@If{}       = Just stmt
isStmt stmt@Let{}      = Just stmt
isStmt stmt@Var{}      = Just stmt
isStmt stmt@While{}    = Just stmt
isStmt UnaryE{}        = Nothing

pattern ExprAST :: AST 'Expr s n t -> AST p s n t
pattern ExprAST expr <- (isExpr -> Just expr)
isExpr :: AST p s n t -> Maybe (AST 'Expr s n t)
isExpr Assign{}           = Nothing
isExpr Block{}            = Nothing
isExpr expr@BinE{}        = Just expr
isExpr expr@Call{}        = Just expr
isExpr expr@FuncExpr{}    = Just expr
isExpr expr@FuncExprAnn{} = Just expr
isExpr expr@Identifier{}  = Just expr
isExpr expr@LiteralE{}    = Just expr
isExpr expr@UnaryE{}      = Just expr
isExpr ExprStmt{}         = Nothing
isExpr FuncDef{}          = Nothing
isExpr FuncDefAnn{}       = Nothing
isExpr If{}               = Nothing
isExpr Let{}              = Nothing
isExpr Program{}          = Nothing
isExpr Var{}              = Nothing
isExpr While{}            = Nothing

pattern ProgramAST :: AST 'TopLevel s n t -> AST p s n t
pattern ProgramAST ast <- (isProgram -> Just ast)
isProgram :: AST p s n t -> Maybe (AST 'TopLevel s n t)
isProgram Assign{}      = Nothing
isProgram ast@Program{} = Just ast
isProgram BinE{}        = Nothing
isProgram Block{}       = Nothing
isProgram Call{}        = Nothing
isProgram ExprStmt{}    = Nothing
isProgram FuncDef{}     = Nothing
isProgram FuncDefAnn{}  = Nothing
isProgram FuncExpr{}    = Nothing
isProgram FuncExprAnn{} = Nothing
isProgram Identifier{}  = Nothing
isProgram If{}          = Nothing
isProgram Let{}         = Nothing
isProgram LiteralE{}    = Nothing
isProgram UnaryE{}      = Nothing
isProgram Var{}         = Nothing
isProgram While{}       = Nothing

pattern FuncDefAST :: AST 'FunctionDef s n t -> AST p s n t
pattern FuncDefAST ast <- (isFuncDef -> Just ast)
isFuncDef :: AST p s n t -> Maybe (AST 'FunctionDef s n t)
isFuncDef Assign{}      = Nothing
isFuncDef ast@FuncDef{}     = Just ast
isFuncDef ast@FuncDefAnn{}  = Just ast
isFuncDef BinE{}        = Nothing
isFuncDef Block{}       = Nothing
isFuncDef Call{}        = Nothing
isFuncDef ExprStmt{}    = Nothing
isFuncDef FuncExpr{}    = Nothing
isFuncDef FuncExprAnn{} = Nothing
isFuncDef Identifier{}  = Nothing
isFuncDef If{}          = Nothing
isFuncDef Let{}         = Nothing
isFuncDef LiteralE{}    = Nothing
isFuncDef Program{}     = Nothing
isFuncDef UnaryE{}      = Nothing
isFuncDef Var{}         = Nothing
isFuncDef While{}       = Nothing

-- Utils 

type family MaybeConstraint (f :: k -> Constraint) (x :: Maybe k) :: Constraint where
    MaybeConstraint f 'Nothing = ()
    MaybeConstraint f ('Just a) = f a
