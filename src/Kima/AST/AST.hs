module Kima.AST.AST where

import Control.Monad.Identity
import Data.Coerce
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Text.Prettyprint.Doc

import GHC.Generics

import Kima.AST.Kinds
import Kima.AST.Names

data AST (part :: ASTPart) tag where
    Program :: [AST 'TopLevel tag] -> AST 'Module tag

    ----------------------- Top-level definitions -----------------------
    FuncDef
        :: Name
        -> [(Name, FreeAnnotation tag)]
        -> FreeAnnotation tag
        -> AST 'Stmt tag
        -> AST 'TopLevel tag
    DataDef
        :: Name
        -> [(Name, FreeAnnotation tag)]
        -> AST 'TopLevel tag

    ----------------------- Expressions -----------------------
    -- Interpreted core
    LiteralE
        :: Literal
        -> AST 'Expr tag
    IdentifierE
        :: Identifier (NameAnnotation tag)
        -> AST 'Expr tag
    FuncExpr
        :: [(Name, FreeAnnotation tag)]
        -> FreeAnnotation tag
        -> AST 'Stmt tag
        -> AST 'Expr tag
    Call
        :: AST 'Expr tag
        -> [AST 'Expr tag]
        -> AST 'Expr tag

    -- Sugar
    AccessE
        :: HasSugar tag
        => AST 'Expr tag
        -> Name
        -> AST 'Expr tag
    BinE
        :: HasSugar tag
        => Binary (AST 'Expr tag)
        -> AST 'Expr tag
    UnaryE
        :: HasSugar tag
        => Unary (AST 'Expr tag)
        -> AST 'Expr tag

    ----------------------- Statements  -----------------------
    -- Interpreted Core
    ExprStmt
        :: AST 'Expr tag
        -> AST 'Stmt tag
    Block
        :: [AST 'Stmt tag]
        -> AST 'Stmt tag
    While
        :: WhileStmt (AST 'Expr tag) (AST 'Stmt tag)
        -> AST 'Stmt tag
    If
        :: IfStmt (AST 'Expr tag) (AST 'Stmt tag)
        -> AST 'Stmt tag
    Assign
        :: WriteAccess (AnnotatedName (NameAnnotation tag))
        -> AST 'Expr tag
        -> AST 'Stmt tag

    -- Typed versions
    Var
        :: Name
        -> FreeAnnotation tag
        -> AST 'Expr tag
        -> AST 'Stmt tag
    Let
        :: Name
        -> FreeAnnotation tag
        -> AST 'Expr tag
        -> AST 'Stmt tag

---------------- Factored out parts of the AST ------------------------------

data Binary e
    = Add e e | Sub e e | Div e e | Mul e e | Mod e e | Less e e | LessEq e e
    | Greater e e | GreatEq e e | Eq e e | NotEq e e
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data Unary e = Negate e | Invert e
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data Literal
    = IntExpr Integer | FloatExpr Double | BoolExpr Bool | StringExpr String
    deriving (Eq, Generic)

data IfStmt cond body = IfStmt {
    cond :: cond,
    ifBlk :: body,
    elseBlk :: body
} deriving (Eq, Generic)

data WhileStmt cond body = WhileStmt {
    cond :: cond,
    body :: body
} deriving (Eq, Generic)

-- | Similar to access but only used for assignment. Consists of a mandatory
-- | base part and an optional list of sub-fields. E.g. @a@ is @WriteAccess "a" []@
-- | and @a.b.c@ is @WriteAccess "a" ["b", "c"]@
data WriteAccess ident = WriteAccess ident [ident]
    deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

-- --------------- Show instances ---------------------
prettyArgList :: (Pretty a, Pretty b) => [(a, b)] -> Doc ann
prettyArgList = tupled . fmap (\(name, t) -> pretty name <> ": " <> pretty t)

instance (Pretty cond, Pretty stmt) => Pretty (IfStmt cond stmt) where
    pretty IfStmt { cond, ifBlk, elseBlk } =
        "if"
            <+> parens (pretty cond)
            <+> pretty ifBlk
            <+> "else"
            <+> pretty elseBlk

instance (Pretty cond, Pretty stmt) => Pretty (WhileStmt cond stmt) where
    pretty WhileStmt { cond, body } =
        "while (" <+> pretty cond <+> ") " <+> pretty body

instance Show Literal where
    show = show . pretty

instance Pretty Literal where
    pretty (IntExpr    n) = "i" <> pretty n
    pretty (FloatExpr  f) = "f" <> pretty f
    pretty (BoolExpr   b) = pretty b
    pretty (StringExpr s) = "s" <> "\"" <> pretty s <> "\""

instance Pretty a => Pretty (Binary a) where
    pretty (Add     l r) = pretty l <+> "+" <+> pretty r
    pretty (Sub     l r) = pretty l <+> "-" <+> pretty r
    pretty (Div     l r) = pretty l <+> "/" <+> pretty r
    pretty (Mul     l r) = pretty l <+> "*" <+> pretty r
    pretty (Mod     l r) = pretty l <+> "%" <+> pretty r
    pretty (Less    l r) = pretty l <+> "<" <+> pretty r
    pretty (LessEq  l r) = pretty l <+> "<=" <+> pretty r
    pretty (Greater l r) = pretty l <+> ">" <+> pretty r
    pretty (GreatEq l r) = pretty l <+> ">=" <+> pretty r
    pretty (Eq      l r) = pretty l <+> "==" <+> pretty r
    pretty (NotEq   l r) = pretty l <+> "!=" <+> pretty r

instance Pretty a => Pretty (Unary a) where
    pretty (Negate e) = "-" <> pretty e
    pretty (Invert e) = "!" <> pretty e

instance (AnnotationConstraint Pretty (NameAnnotation tag),
          Pretty (AnnotatedName (NameAnnotation tag)),
          Pretty (FreeAnnotation tag)) =>
         Show (AST p tag) where
    show = show . pretty

instance (AnnotationConstraint Pretty (NameAnnotation tag),
          Pretty (AnnotatedName (NameAnnotation tag)),
          Pretty (FreeAnnotation tag)) =>
         Pretty (AST p tag) where
    pretty (Program ast) = vcat (pretty <$> ast)
    pretty (FuncDef name sig rt body) =
        "fun"
            <+> pretty name
            <>  prettyArgList sig
            <+> "->"
            <+> pretty rt
            <+> pretty body
    pretty (FuncExpr sig rt body) =
        "fun" <+> prettyArgList sig <+> "->" <+> pretty rt <+> pretty body
    pretty (DataDef name members) =
        "data"
            <+> pretty name
            <+> "{"
            <>  line
            <>  vcat
                    (punctuate ","
                        ((\(n, t) -> pretty n <> ": " <> pretty t) <$> members)
                    )
            <>  line
            <>  "}"
    pretty (Var name t expr) =
        "var" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (Let name t expr) =
        "let" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (LiteralE    lit ) = pretty lit
    pretty (IdentifierE name) = pretty name
    pretty (Call callee args) = pretty callee <> tupled (pretty <$> args)
    pretty (BinE     bin    ) = pretty bin
    pretty (UnaryE   unary  ) = pretty unary
    pretty (AccessE  expr name) = parens (pretty expr) <> "." <> pretty name
    pretty (ExprStmt expr   ) = pretty expr
    pretty (Block stmts) =
        "{" <> line <> indent 4 (vcat (pretty <$> stmts)) <> line <> "}"
    pretty (Assign name expr) = pretty name <+> "=" <+> pretty expr
    pretty (While stmt      ) = pretty stmt
    pretty (If    stmt      ) = pretty stmt

instance Pretty ident => Pretty (WriteAccess ident) where
    pretty (WriteAccess ident rest) =
        hcat $ punctuate "." (pretty <$> ident : rest)

instance Pretty ident => Show (WriteAccess ident) where
    show = show . pretty

--------------- Boring instances ---------------------

instance Bifunctor IfStmt where
    bimap f g IfStmt { cond, ifBlk, elseBlk } =
        IfStmt (f cond) (g ifBlk) (g elseBlk)

instance Bifoldable IfStmt where
    bifoldMap f g IfStmt { cond, ifBlk, elseBlk } =
        f cond <> g ifBlk <> g elseBlk

instance Bitraversable IfStmt where
    bitraverse f g IfStmt { cond, ifBlk, elseBlk } =
        (\(a, b, c) -> IfStmt a b c)
            <$> ((,,) <$> f cond <*> g ifBlk <*> g elseBlk)

instance Bifunctor WhileStmt where
    bimap f g WhileStmt { cond, body } = WhileStmt (f cond) (g body)

instance Bifoldable WhileStmt where
    bifoldMap f g WhileStmt { cond, body } = f cond <> g body

instance Bitraversable WhileStmt where
    bitraverse f g WhileStmt { cond, body } =
        uncurry WhileStmt <$> bitraverse f g (cond, body)

deriving instance ( AnnotationConstraint Eq (NameAnnotation tag)
                  , Eq (AnnotatedName (NameAnnotation tag))
                  , Eq (FreeAnnotation tag)) => Eq (AST p tag)

-- -- Traverals
mapFreeAnnotations
    :: forall t1 t2 p
    . (TagSugar t1 ~ TagSugar t2
    ,  NameAnnotation t1 ~ NameAnnotation t2)
    => (FreeAnnotation t1 -> FreeAnnotation t2)
    -> AST p t1
    -> AST p t2
mapFreeAnnotations = coerce
    (traverseFreeAnnotations
        :: (FreeAnnotation t1 -> Identity (FreeAnnotation t2))
        -> AST p t1
        -> Identity (AST p t2))

traverseFreeAnnotations
    :: ( TagSugar t1 ~ TagSugar t2
       , NameAnnotation t1 ~ NameAnnotation t2
       , Applicative m)
    => (FreeAnnotation t1 -> m (FreeAnnotation t2))
    -> AST p t1
    -> m (AST p t2)
traverseFreeAnnotations f (Var n t e) = Var n
    <$> f t
    <*> traverseFreeAnnotations f e
traverseFreeAnnotations f (Let n t e) = Let n
    <$> f t
    <*> traverseFreeAnnotations f e
traverseFreeAnnotations f (FuncDef n args rt b) = FuncDef n
    <$> traverse (traverse f) args
    <*> f rt
    <*> traverseFreeAnnotations f b
traverseFreeAnnotations f (FuncExpr args rt b) = FuncExpr
    <$> traverse (traverse f) args
    <*> f rt
    <*> traverseFreeAnnotations f b
traverseFreeAnnotations f (Program ast      ) = Program
    <$> traverse (traverseFreeAnnotations f) ast
traverseFreeAnnotations f (DataDef n members) = DataDef n
    <$> traverse (traverse f) members
traverseFreeAnnotations f (Call callee args) = Call
    <$> traverseFreeAnnotations f callee
    <*> traverse (traverseFreeAnnotations f) args
traverseFreeAnnotations f (AccessE expr name) = AccessE
    <$> traverseFreeAnnotations f expr
    <*> pure name
traverseFreeAnnotations f (BinE bin) = BinE
    <$> traverse (traverseFreeAnnotations f) bin
traverseFreeAnnotations f (UnaryE unary) = UnaryE
    <$> traverse (traverseFreeAnnotations f) unary
traverseFreeAnnotations f (ExprStmt e) = ExprStmt
    <$> traverseFreeAnnotations f e
traverseFreeAnnotations f (Block blk) = Block
    <$> traverse (traverseFreeAnnotations f) blk
traverseFreeAnnotations f (While stmt) = While
    <$> bitraverse (traverseFreeAnnotations f) (traverseFreeAnnotations f) stmt
traverseFreeAnnotations f (If stmt) = If
    <$> bitraverse (traverseFreeAnnotations f) (traverseFreeAnnotations f) stmt
traverseFreeAnnotations f (Assign n e) = Assign n
    <$> traverseFreeAnnotations f e
traverseFreeAnnotations _ (LiteralE lit) = pure $ LiteralE lit
traverseFreeAnnotations _ (IdentifierE n) = pure $ IdentifierE n

-- | Traverse the AST, adding annotations to all identifiers drawn from an applicative action
addIdAnnotations
    :: ( Applicative m
       , TagSugar t1 ~ TagSugar t2
       , FreeAnnotation t1 ~ FreeAnnotation t2
       , NameAnnotation t1 ~ 'NoAnnotation
       , NameAnnotation t2 ~ 'Annotation idAnn)
    => m idAnn -- ^ Applicative action producing Annotations
    -> AST p t1 -- ^ Original AST
    -> m (AST p t2)
addIdAnnotations f (Assign access e) = Assign
    <$> traverse (\(Name n) -> TName n <$> f) access
    <*> addIdAnnotations f e
addIdAnnotations f (Program ast) = Program
    <$> traverse (addIdAnnotations f) ast
addIdAnnotations f (ExprStmt e) = ExprStmt
    <$> addIdAnnotations f e
addIdAnnotations _ (LiteralE lit) = pure $ LiteralE lit
addIdAnnotations f (IdentifierE n) = IdentifierE
    <$> (typeAnnotate <$> f <*> pure n)
addIdAnnotations f (BinE bin) = BinE
    <$> traverse (addIdAnnotations f) bin
addIdAnnotations f (UnaryE unary) = UnaryE
    <$> traverse (addIdAnnotations f) unary
addIdAnnotations f (While stmt) = While
    <$> bitraverse (addIdAnnotations f) (addIdAnnotations f) stmt
addIdAnnotations f (If stmt) = If
    <$> bitraverse (addIdAnnotations f) (addIdAnnotations f) stmt
addIdAnnotations f (Block blk) = Block
    <$> traverse (addIdAnnotations f) blk
addIdAnnotations f (FuncDef n args rt b) = FuncDef n args rt
    <$> addIdAnnotations f b
addIdAnnotations f (FuncExpr args rt b) = FuncExpr args rt
    <$> addIdAnnotations f b
addIdAnnotations _ (DataDef n members) = pure $ DataDef n members
addIdAnnotations f (Call callee args) = Call
    <$> addIdAnnotations f callee
    <*> traverse (addIdAnnotations f) args
addIdAnnotations f (AccessE expr name) = AccessE
    <$> addIdAnnotations f expr
    <*> pure name
addIdAnnotations f (Var n t e) = Var n t <$> addIdAnnotations f e
addIdAnnotations f (Let n t e) = Let n t <$> addIdAnnotations f e

-- | Traverse annotations on identifiers
traverseIdAnnotations
    :: ( Applicative m
       , TagSugar t1 ~ TagSugar t2
       , FreeAnnotation t1 ~ FreeAnnotation t2
       , NameAnnotation t1 ~ 'Annotation idAnn1
       , NameAnnotation t2 ~ 'Annotation idAnn2)
    => (idAnn1 -> m idAnn2)
    -> AST p t1
    -> m (AST p t2)
traverseIdAnnotations f (Program ast) = Program
    <$> traverse (traverseIdAnnotations f) ast
traverseIdAnnotations f (ExprStmt e) = ExprStmt
    <$> traverseIdAnnotations f e
traverseIdAnnotations _ (LiteralE lit) = pure $ LiteralE lit
traverseIdAnnotations f (IdentifierE n) = IdentifierE
    <$> traverseAnnotation f n
traverseIdAnnotations f (BinE bin) = BinE
    <$> traverse (traverseIdAnnotations f) bin
traverseIdAnnotations f (UnaryE unary) = UnaryE
    <$> traverse (traverseIdAnnotations f) unary
traverseIdAnnotations f (While stmt) = While
    <$> bitraverse (traverseIdAnnotations f) (traverseIdAnnotations f) stmt
traverseIdAnnotations f (If stmt) = If
    <$> bitraverse (traverseIdAnnotations f) (traverseIdAnnotations f) stmt
traverseIdAnnotations f (Block blk) = Block
    <$> traverse (traverseIdAnnotations f) blk
traverseIdAnnotations f (FuncDef n args rt b) = FuncDef n args rt
    <$> traverseIdAnnotations f b
traverseIdAnnotations f (FuncExpr args rt b) = FuncExpr args rt
    <$> traverseIdAnnotations f b
traverseIdAnnotations _ (DataDef n members) = pure $ DataDef n members
traverseIdAnnotations f (Call callee args) = Call
    <$> traverseIdAnnotations f callee
    <*> traverse (traverseIdAnnotations f) args
traverseIdAnnotations f (AccessE expr name) = AccessE
    <$> traverseIdAnnotations f expr
    <*> pure name
traverseIdAnnotations f (Assign access e) = Assign
    <$> traverse (\(TName n t) -> TName n <$> f t) access
    <*> traverseIdAnnotations f e
traverseIdAnnotations f (Var n t e) = Var n t
    <$> traverseIdAnnotations f e
traverseIdAnnotations f (Let n t e) = Let n t
    <$> traverseIdAnnotations f e

-- -- Patterns
{-# COMPLETE StmtAST, ExprAST, ProgramAST, TopLevelAST#-}

pattern StmtAST :: AST 'Stmt t -> AST p t
pattern StmtAST stmt <- (isStmt -> Just stmt)

isStmt :: AST p t -> Maybe (AST 'Stmt t)
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

pattern ExprAST :: AST 'Expr t -> AST p t
pattern ExprAST expr <- (isExpr -> Just expr)
isExpr :: AST p t -> Maybe (AST 'Expr t)
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

pattern ProgramAST :: AST 'Module t -> AST p t
pattern ProgramAST ast <- (isProgram -> Just ast)
isProgram :: AST p t -> Maybe (AST 'Module t)
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

pattern TopLevelAST :: AST 'TopLevel t -> AST p t
pattern TopLevelAST ast <- (isTopLevelAST -> Just ast)
isTopLevelAST :: AST p t -> Maybe (AST 'TopLevel t)
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
