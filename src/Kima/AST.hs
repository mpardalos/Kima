module Kima.AST where

import Control.Monad.Identity
import Data.Coerce
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Kind
import Data.String
import Data.Text.Prettyprint.Doc

import GHC.Generics

import Kima.KimaTypes

---------- Names (identifiers) ---------- 
data BuiltinName = AddOp | SubOp | MulOp | ModOp | DivOp  -- Binary ops
                 | GTOp | GTEOp | LTOp | LTEOp | EqualsOp
                 | InvertOp | NegateOp -- Unary ops
                 | PrintFunc | InputFunc -- Builtin functions
    deriving (Show, Eq, Ord, Generic)

type TypeName        = String

data Identifier :: HasAnnotation -> Type where
    -- Strings
    Identifier   :: String      -> Identifier 'NoAnnotation
    TIdentifier  :: String -> t -> Identifier ('Annotation t)

    -- Builtins
    Builtin     :: BuiltinName -> Identifier 'NoAnnotation
    TBuiltin    :: BuiltinName -> t -> Identifier ('Annotation t)

    -- Accessors
    Accessor    :: String      -> Identifier 'NoAnnotation
    TAccessor   :: String -> t -> Identifier ('Annotation t)
    
---------- AST ----------  
data Sugar = Sugar | NoSugar
data ASTPart = Expr | Stmt | TopLevel | Module

data Binary e = Add e e | Sub e e | Div e e | Mul e e | Mod e e | Less e e 
              | LessEq e e | Greater e e | GreatEq e e | Eq e e | NotEq e e
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data Unary e = Negate e | Invert e
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data Literal = IntExpr Integer | FloatExpr Double | BoolExpr Bool | StringExpr String 
    deriving (Eq, Generic)

data IfStmt cond body = IfStmt {
    cond :: cond,
    ifBlk :: body,
    elseBlk :: body
} deriving Eq

data WhileStmt cond body = WhileStmt {
    cond :: cond,
    body :: body
} deriving Eq

data Access ident expr = Access expr Name | IdAccess ident
    deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

data WriteAccess ident = WriteAccess ident [ident]
    deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

data HasAnnotation = NoAnnotation | Annotation Type
type Name = String
-- TODO Unify AnnotatedName, Name and Identifier under a typeclass
data AnnotatedName :: HasAnnotation -> Type where
    Name  :: String      -> AnnotatedName 'NoAnnotation
    TName :: String -> t -> AnnotatedName ('Annotation t)

data AST (part :: ASTPart) (sugar :: Sugar) (idAnn :: HasAnnotation) (typeAnn :: Type) where
    Program :: [AST 'TopLevel s i t] -> AST 'Module s i t

    ----------------------- Top-level definitions ----------------------- 
    FuncDef :: Name -> [(Name, t)] -> t -> AST 'Stmt s i t -> AST 'TopLevel s i t
    DataDef :: Name -> [(Name, t)]                         -> AST 'TopLevel s i t

    ----------------------- Expressions ----------------------- 
    -- Interpreted core
    LiteralE    :: Literal                              -> AST 'Expr s i t
    IdentifierE :: Identifier i                         -> AST 'Expr s i t
    FuncExpr    :: [(Name, t)] -> t -> AST 'Stmt s i t  -> AST 'Expr s i t
    Call        :: AST 'Expr s i t -> [AST 'Expr s i t] -> AST 'Expr s i t

    -- Sugar
    AccessE :: Access (Identifier i) (AST 'Expr 'Sugar i t) -> AST 'Expr 'Sugar i t
    BinE    :: Binary (AST 'Expr 'Sugar i t)                -> AST 'Expr 'Sugar i t
    UnaryE  :: Unary (AST 'Expr 'Sugar i t)                 -> AST 'Expr 'Sugar i t

    ----------------------- Statements  ----------------------- 
    -- Interpreted Core
    ExprStmt :: AST 'Expr s i t                                  -> AST 'Stmt s i t
    Block    :: [AST 'Stmt s i t]                                -> AST 'Stmt s i t
    While    :: WhileStmt (AST 'Expr s i t) (AST 'Stmt s i t)    -> AST 'Stmt s i t
    If       :: IfStmt (AST 'Expr s i t) (AST 'Stmt s i t)       -> AST 'Stmt s i t
    Assign   :: WriteAccess (AnnotatedName i) -> AST 'Expr s i t -> AST 'Stmt s i t

    -- Typed versions
    Var      :: Name -> t -> AST 'Expr s i t -> AST 'Stmt s i t
    Let      :: Name -> t -> AST 'Expr s i t -> AST 'Stmt s i t

------ Type synonyms for different phases -----
-- > Parse ->
type ParsedAST        (p :: ASTPart) = AST p 'Sugar   'NoAnnotation       TypeExpr
-- > Desugar ->
type DesugaredAST     (p :: ASTPart) = AST p 'NoSugar 'NoAnnotation       TypeExpr
-- > Resolve Types ->
type TypeAnnotatedAST (p :: ASTPart) = AST p 'NoSugar 'NoAnnotation       KType
-- > Typecheck ->
type TypedAST         (p :: ASTPart) = AST p 'NoSugar ('Annotation KType) KType
type RuntimeAST       (p :: ASTPart) = TypedAST p

type ParsedProgram        = ParsedAST        'Module
type DesugaredProgram     = DesugaredAST     'Module
type TypeAnnotatedProgram = TypeAnnotatedAST 'Module
type TypedProgram         = TypedAST         'Module
type RuntimeProgram       = RuntimeAST       'Module

-- Types
data TypeExpr = TypeName TypeName
              | SignatureType [TypeExpr] TypeExpr
    deriving Eq

--------- Useful functions ----------
typeAnnotate :: t -> Identifier a -> Identifier ('Annotation t)
typeAnnotate t (Identifier  n  ) = TIdentifier n t
typeAnnotate t (TIdentifier n _) = TIdentifier n t
typeAnnotate t (Builtin     n  ) = TBuiltin n t
typeAnnotate t (TBuiltin    n _) = TBuiltin n t
typeAnnotate t (Accessor    n  ) = TAccessor n t
typeAnnotate t (TAccessor   n _) = TAccessor n t

deTypeAnnotate :: Identifier ('Annotation t)  -> Identifier 'NoAnnotation
deTypeAnnotate (TIdentifier n _) = Identifier n
deTypeAnnotate (TBuiltin    n _) = Builtin n
deTypeAnnotate (TAccessor   n _) = Accessor n

nameType :: Identifier ('Annotation t) -> t
nameType (TIdentifier _ t) = t
nameType (TBuiltin    _ t) = t
nameType (TAccessor   _ t) = t

toIdentifier :: AnnotatedName t -> Identifier t
toIdentifier (Name n) = Identifier n
toIdentifier (TName n t) = TIdentifier n t

--------------- Show instances ---------------------
prettyArgList :: (Pretty a, Pretty b) => [(a, b)] -> Doc ann
prettyArgList = tupled . fmap (\(name, t) -> pretty name <> ": " <> pretty t)

instance (Pretty cond, Pretty stmt) => Pretty (IfStmt cond stmt) where
    pretty IfStmt { cond, ifBlk, elseBlk } = 
        "if" <+> parens (pretty cond) <+> pretty ifBlk <+> "else" <+> pretty elseBlk

instance (Pretty cond, Pretty stmt) => Pretty (WhileStmt cond stmt) where
    pretty WhileStmt { cond, body } = "while (" <+> pretty cond <+> ") " <+> pretty body

instance Show Literal where
    show = show . pretty
instance Pretty Literal where
    pretty (IntExpr n    ) = "i" <> pretty n
    pretty (FloatExpr f  ) = "f" <> pretty f
    pretty (BoolExpr b   ) = pretty b
    pretty (StringExpr s ) = "s" <> "\"" <> pretty s <> "\""

instance Pretty a => Pretty (Binary a) where
    pretty (Add     l r) = pretty l <+> "+"  <+> pretty r
    pretty (Sub     l r) = pretty l <+> "-"  <+> pretty r
    pretty (Div     l r) = pretty l <+> "/"  <+> pretty r
    pretty (Mul     l r) = pretty l <+> "*"  <+> pretty r
    pretty (Mod     l r) = pretty l <+> "%"  <+> pretty r
    pretty (Less    l r) = pretty l <+> "<"  <+> pretty r
    pretty (LessEq  l r) = pretty l <+> "<=" <+> pretty r
    pretty (Greater l r) = pretty l <+> ">"  <+> pretty r
    pretty (GreatEq l r) = pretty l <+> ">=" <+> pretty r
    pretty (Eq      l r) = pretty l <+> "==" <+> pretty r
    pretty (NotEq   l r) = pretty l <+> "!=" <+> pretty r
instance Pretty a => Pretty (Unary a) where
    pretty (Negate e) = "-" <> pretty e
    pretty (Invert e) = "!" <> pretty e

instance (AnnotationConstraint Pretty i,
          Pretty (AnnotatedName i),
          Pretty t) =>
         Show (AST p s i t) where
    show = show . pretty

instance (AnnotationConstraint Pretty i,
          Pretty (AnnotatedName i),
          Pretty t) =>
         Pretty (AST p s i t) where
    pretty (Program ast) = vcat (pretty <$> ast)
    pretty (FuncDef name sig rt body) =
        "fun" <+> pretty name <> prettyArgList sig <+> "->" <+> pretty rt <+> pretty body
    pretty (FuncExpr sig rt body) =
        "fun" <+> prettyArgList sig <+> "->" <+> pretty rt <+> pretty body
    pretty (DataDef name members) =
        "data" <+> pretty name <+> "{" <> line <>
            vcat (punctuate ","
            ((\(n, t) -> pretty n <> ": " <> pretty t) <$> members))
        <> line <> "}"
    pretty (Var name t expr) = "var" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (Let name t expr) = "let" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (LiteralE lit) = pretty lit
    pretty (IdentifierE name) = pretty name
    pretty (Call callee args) = pretty callee <> tupled (pretty <$> args)
    pretty (BinE bin) = pretty bin
    pretty (UnaryE unary) = pretty unary
    pretty (AccessE access) = pretty access
    pretty (ExprStmt expr) = pretty expr
    pretty (Block stmts) = "{" <> line
        <> indent 4 (vcat (pretty <$> stmts))
        <> line <> "}"
    pretty (Assign name expr) = pretty name <+> "=" <+> pretty expr
    pretty (While stmt) = pretty stmt 
    pretty (If stmt) = pretty stmt

instance Show TypeExpr where
    show (TypeName s) = "#\"" ++ s ++ "\""
    show (SignatureType args rt) = "#( (" ++ show args ++ ") -> " ++ show rt ++ ")"

instance Pretty TypeExpr where
    pretty (TypeName name) = pretty name
    pretty (SignatureType args returnType) =
        tupled (pretty <$> args) <+> "->" <+> pretty returnType

instance (AnnotationConstraint Show t) => Show (Identifier t) where
    show (TIdentifier n t) = "{"  ++ n      ++ " : " ++ show t ++ "}"
    show (TBuiltin n t   ) = "{"  ++ show n ++ " : " ++ show t ++ "}"
    show (TAccessor n t  ) = "{." ++ show n ++ " : " ++ show t ++ "}"
    show (Identifier  str) = "{"  ++ str    ++ "}"
    show (Builtin n      ) = "{"  ++ show n ++ "}"
    show (Accessor str   ) = "{." ++ str    ++ "}"

instance (AnnotationConstraint Show t) => Show (AnnotatedName t) where
    show (TName n t) = "{"  ++ n   ++ " : " ++ show t ++ "}"
    show (Name  str) = "{"  ++ str ++ "}"

instance Pretty BuiltinName where
    pretty AddOp     = "(+)"
    pretty SubOp     = "(-)"
    pretty MulOp     = "(*)"
    pretty ModOp     = "(%)"
    pretty DivOp     = "(/)"
    pretty GTOp      = "(>)"
    pretty GTEOp     = "(>=)"
    pretty LTOp      = "(<)"
    pretty LTEOp     = "(<=)"
    pretty EqualsOp  = "(==)"
    pretty InvertOp  = "(-)"
    pretty NegateOp  = "(!)"
    pretty PrintFunc = "b'print"
    pretty InputFunc = "b'input"

instance Pretty ident => Pretty (WriteAccess ident) where
    pretty (WriteAccess ident rest) = hcat $ punctuate "." (pretty <$> ident:rest)
instance Pretty ident => Show (WriteAccess ident) where
    show = show . pretty

instance (Pretty ident, Pretty expr) => Pretty (Access ident expr) where
    pretty (Access record field) = pretty record <> "." <> pretty field
    pretty (IdAccess identifier) = pretty identifier

instance AnnotationConstraint Pretty t => Pretty (AnnotatedName t) where
    pretty (TName str t) = "{"  <> fromString str <+> ":" <+> pretty t <> "}"
    pretty (Name str) = "{" <> fromString str <> "}"

instance AnnotationConstraint Pretty t => Pretty (Identifier t) where
    pretty (TIdentifier str t) = "{"  <> fromString str <+> ":" <+> pretty t <> "}"
    pretty (TBuiltin n t)    = "{"  <> pretty n       <+> ":" <+> pretty t <> "}"
    pretty (TAccessor n t)   = "{." <> pretty n       <+> ":" <+> pretty t <> "}"
    pretty (Identifier str) = "{" <> fromString str <> "}"
    pretty (Builtin n) = "{" <> pretty n <> "}"
    pretty (Accessor n) = "{." <> fromString n <> "}"

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

instance Bifunctor Access where
    bimap f _ (IdAccess      ident) = IdAccess (f ident)
    bimap _ g (Access record field) = Access (g record) field

instance Bifoldable Access where
    bifoldMap f _ (IdAccess  ident) = f ident
    bifoldMap _ g (Access record _) = g record

instance Bitraversable Access where
    bitraverse f _ (IdAccess      ident) = IdAccess <$> f ident
    bitraverse _ g (Access record field) = Access <$> g record <*> pure field

instance IsString (Identifier 'NoAnnotation) where
    fromString ('.':name) = Accessor name
    fromString name       = Identifier name

instance IsString (AnnotatedName 'NoAnnotation) where
    fromString = Name

deriving instance AnnotationConstraint Eq t => Eq (Identifier t)
deriving instance AnnotationConstraint Eq t => Eq (AnnotatedName t)
deriving instance (AnnotationConstraint Eq t, AnnotationConstraint Ord t) => Ord (Identifier t)
deriving instance (AnnotationConstraint Eq i,
                   Eq (AnnotatedName i),
                   Eq t) => Eq (AST p s i t)

-- Traverals

traverseAnnotation :: Functor m => (t1 -> m t2) -> Identifier ('Annotation t1) -> m (Identifier ('Annotation t2))
traverseAnnotation f (TIdentifier n t) = TIdentifier n <$> f t
traverseAnnotation f (TBuiltin    n t) = TBuiltin    n <$> f t
traverseAnnotation f (TAccessor   n t) = TAccessor   n <$> f t

mapTypeAnnotations :: forall t1 t2 p s i. (t1 -> t2) -> AST p s i t1 -> AST p s i t2
mapTypeAnnotations = coerce (traverseTypeAnnotations :: (t1 -> Identity t2) -> AST p s i t1 -> Identity (AST p s i t2))

traverseTypeAnnotations :: Applicative m => (t1 -> m t2) -> AST p s i t1 -> m (AST p s i t2)
traverseTypeAnnotations f (Program ast)         = Program <$> traverse (traverseTypeAnnotations f) ast
traverseTypeAnnotations f (DataDef n members)   = DataDef n <$> traverse (traverse f) members
traverseTypeAnnotations f (FuncDef n args rt b) = FuncDef n <$> traverse (traverse f) args <*> f rt <*> traverseTypeAnnotations f b
traverseTypeAnnotations _ (LiteralE lit)        = pure $ LiteralE lit
traverseTypeAnnotations _ (IdentifierE n)       = pure $ IdentifierE n
traverseTypeAnnotations f (FuncExpr args rt b)  = FuncExpr <$> traverse (traverse f) args       <*> f rt <*> traverseTypeAnnotations f b
traverseTypeAnnotations f (Call callee args)    = Call     <$> traverseTypeAnnotations f callee <*> traverse (traverseTypeAnnotations f) args
traverseTypeAnnotations f (AccessE access)      = AccessE  <$> bitraverse pure (traverseTypeAnnotations f) access
traverseTypeAnnotations f (BinE bin)            = BinE     <$> traverse (traverseTypeAnnotations f) bin
traverseTypeAnnotations f (UnaryE unary)        = UnaryE   <$> traverse (traverseTypeAnnotations f) unary
traverseTypeAnnotations f (ExprStmt e)          = ExprStmt <$> traverseTypeAnnotations f e
traverseTypeAnnotations f (Block blk)           = Block    <$> traverse (traverseTypeAnnotations f) blk
traverseTypeAnnotations f (While stmt)          = While    <$> bitraverse (traverseTypeAnnotations f) (traverseTypeAnnotations f) stmt
traverseTypeAnnotations f (If stmt)             = If       <$> bitraverse (traverseTypeAnnotations f) (traverseTypeAnnotations f) stmt
traverseTypeAnnotations f (Assign n e)          = Assign n <$> traverseTypeAnnotations f e
traverseTypeAnnotations f (Var n t e)           = Var n    <$> f t <*> traverseTypeAnnotations f e
traverseTypeAnnotations f (Let n t e)           = Let n    <$> f t <*> traverseTypeAnnotations f e

addIdAnnotations :: Applicative m => m idAnn -> AST p sug 'NoAnnotation t -> m (AST p sug ('Annotation idAnn) t)
addIdAnnotations f (Program ast)         = Program <$> traverse (addIdAnnotations f) ast
addIdAnnotations f (ExprStmt e)          = ExprStmt <$> addIdAnnotations f e
addIdAnnotations _ (LiteralE lit)        = pure $ LiteralE lit
addIdAnnotations f (IdentifierE n)          = IdentifierE <$> (typeAnnotate <$> f <*> pure n)
addIdAnnotations f (BinE bin)            = BinE   <$> traverse (addIdAnnotations f) bin
addIdAnnotations f (UnaryE unary)        = UnaryE <$> traverse (addIdAnnotations f) unary
addIdAnnotations f (While stmt)          = While  <$> bitraverse (addIdAnnotations f) (addIdAnnotations f) stmt
addIdAnnotations f (If stmt)             = If     <$> bitraverse (addIdAnnotations f) (addIdAnnotations f) stmt
addIdAnnotations f (Block blk)           = Block  <$> traverse (addIdAnnotations f) blk
addIdAnnotations f (FuncDef n args rt b) = FuncDef  n args rt <$> addIdAnnotations f b
addIdAnnotations f (FuncExpr args rt b)  = FuncExpr args rt <$> addIdAnnotations f b
addIdAnnotations _ (DataDef n members)   = pure $ DataDef n members
addIdAnnotations f (Call callee args)    = Call    <$> addIdAnnotations f callee <*> traverse (addIdAnnotations f) args
addIdAnnotations f (AccessE access)      = AccessE <$> bitraverse (\n -> typeAnnotate <$> f <*> pure n) (addIdAnnotations f) access
addIdAnnotations f (Assign access e)     = Assign  <$> traverse (\(Name n) -> TName n <$> f) access <*> addIdAnnotations f e
addIdAnnotations f (Var n t e)           = Var n t <$> addIdAnnotations f e
addIdAnnotations f (Let n t e)           = Let n t <$> addIdAnnotations f e

traverseIdAnnotations :: Applicative m
                      => (idAnn1 -> m idAnn2)
                      ->    AST p sug ('Annotation idAnn1) t
                      -> m (AST p sug ('Annotation idAnn2) t)
traverseIdAnnotations f (Program ast)            = Program <$> traverse (traverseIdAnnotations f) ast
traverseIdAnnotations f (ExprStmt e)             = ExprStmt <$> traverseIdAnnotations f e
traverseIdAnnotations _ (LiteralE lit)           = pure $ LiteralE lit
traverseIdAnnotations f (IdentifierE n)          = IdentifierE <$> traverseAnnotation f n
traverseIdAnnotations f (BinE bin)               = BinE   <$> traverse (traverseIdAnnotations f) bin
traverseIdAnnotations f (UnaryE unary)           = UnaryE <$> traverse (traverseIdAnnotations f) unary
traverseIdAnnotations f (While stmt)             = While  <$> bitraverse (traverseIdAnnotations f) (traverseIdAnnotations f) stmt
traverseIdAnnotations f (If stmt)                = If     <$> bitraverse (traverseIdAnnotations f) (traverseIdAnnotations f) stmt
traverseIdAnnotations f (Block blk)              = Block  <$> traverse (traverseIdAnnotations f) blk
traverseIdAnnotations f (FuncDef n args rt b)    = FuncDef  n args rt <$> traverseIdAnnotations f b
traverseIdAnnotations f (FuncExpr args rt b)     = FuncExpr args rt <$> traverseIdAnnotations f b
traverseIdAnnotations _ (DataDef n members)      = pure $ DataDef n members
traverseIdAnnotations f (Call callee args)       = Call    <$> traverseIdAnnotations f callee <*> traverse (traverseIdAnnotations f) args
traverseIdAnnotations f (AccessE access)         = AccessE <$> bitraverse (traverseAnnotation f) (traverseIdAnnotations f) access
traverseIdAnnotations f (Assign access e)        = Assign  <$> traverse (\(TName n t) -> TName n <$> f t) access <*> traverseIdAnnotations f e
traverseIdAnnotations f (Var n t e)              = Var n t <$> traverseIdAnnotations f e
traverseIdAnnotations f (Let n t e)              = Let n t <$> traverseIdAnnotations f e

-- Patterns
{-# COMPLETE StmtAST, ExprAST, ProgramAST, TopLevelAST#-}

pattern StmtAST :: AST 'Stmt s n t -> AST p s n t
pattern StmtAST stmt <- (isStmt -> Just stmt)

isStmt :: AST p s n t -> Maybe (AST 'Stmt s n t)
isStmt stmt@Assign{}   = Just stmt
isStmt BinE{}          = Nothing
isStmt stmt@Block{}    = Just stmt
isStmt Call{}          = Nothing
isStmt AccessE{}       = Nothing
isStmt DataDef{}       = Nothing
isStmt stmt@ExprStmt{} = Just stmt
isStmt FuncDef{}       = Nothing
isStmt FuncExpr{}      = Nothing
isStmt IdentifierE{}    = Nothing
isStmt stmt@If{}       = Just stmt
isStmt stmt@Let{}      = Just stmt
isStmt LiteralE{}      = Nothing
isStmt Program{}       = Nothing
isStmt UnaryE{}        = Nothing
isStmt stmt@Var{}      = Just stmt
isStmt stmt@While{}    = Just stmt

pattern ExprAST :: AST 'Expr s n t -> AST p s n t
pattern ExprAST expr <- (isExpr -> Just expr)
isExpr :: AST p s n t -> Maybe (AST 'Expr s n t)
isExpr Assign{}           = Nothing
isExpr expr@BinE{}        = Just expr
isExpr Block{}            = Nothing
isExpr expr@Call{}        = Just expr
isExpr expr@AccessE{}     = Just expr
isExpr DataDef{}          = Nothing
isExpr ExprStmt{}         = Nothing
isExpr FuncDef{}          = Nothing
isExpr expr@FuncExpr{}    = Just expr
isExpr expr@IdentifierE{} = Just expr
isExpr If{}               = Nothing
isExpr Let{}              = Nothing
isExpr expr@LiteralE{}    = Just expr
isExpr Program{}          = Nothing
isExpr expr@UnaryE{}      = Just expr
isExpr Var{}              = Nothing
isExpr While{}            = Nothing

pattern ProgramAST :: AST 'Module s n t -> AST p s n t
pattern ProgramAST ast <- (isProgram -> Just ast)
isProgram :: AST p s n t -> Maybe (AST 'Module s n t)
isProgram Assign{}      = Nothing
isProgram BinE{}        = Nothing
isProgram Block{}       = Nothing
isProgram Call{}        = Nothing
isProgram AccessE{}     = Nothing
isProgram DataDef{}     = Nothing
isProgram ExprStmt{}    = Nothing
isProgram FuncDef{}     = Nothing
isProgram FuncExpr{}    = Nothing
isProgram IdentifierE{} = Nothing
isProgram If{}          = Nothing
isProgram Let{}         = Nothing
isProgram LiteralE{}    = Nothing
isProgram ast@Program{} = Just ast
isProgram UnaryE{}      = Nothing
isProgram Var{}         = Nothing
isProgram While{}       = Nothing

pattern TopLevelAST :: AST 'TopLevel s n t -> AST p s n t
pattern TopLevelAST ast <- (isTopLevelAST -> Just ast)
isTopLevelAST :: AST p s n t -> Maybe (AST 'TopLevel s n t)
isTopLevelAST Assign{}         = Nothing
isTopLevelAST BinE{}           = Nothing
isTopLevelAST Block{}          = Nothing
isTopLevelAST Call{}           = Nothing
isTopLevelAST AccessE{}        = Nothing
isTopLevelAST ast@DataDef{}    = Just ast
isTopLevelAST ExprStmt{}       = Nothing
isTopLevelAST ast@FuncDef{}    = Just ast
isTopLevelAST FuncExpr{}       = Nothing
isTopLevelAST IdentifierE{}    = Nothing
isTopLevelAST If{}             = Nothing
isTopLevelAST Let{}            = Nothing
isTopLevelAST LiteralE{}       = Nothing
isTopLevelAST Program{}        = Nothing
isTopLevelAST UnaryE{}         = Nothing
isTopLevelAST Var{}            = Nothing
isTopLevelAST While{}          = Nothing

-- Utils 

type family AnnotationConstraint (f :: k -> Constraint) (x :: HasAnnotation) :: Constraint where
    AnnotationConstraint f 'NoAnnotation   = ()
    AnnotationConstraint f ('Annotation a) = f a
