module Kima.AST.AST where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Text.Prettyprint.Doc

import GHC.Generics

import Kima.AST.Kinds
import Kima.AST.Names

data AST tag
    = ModuleAST (Module tag)
    | TopLevelAST (TopLevel tag)
    | ExprAST (Expr tag)
    | StmtAST (Stmt tag)

data Module tag = Program [TopLevel tag]

data TopLevel tag
    = FuncDef Name [(Name, FreeAnnotation tag)] (EffectType tag) (FreeAnnotation tag) (Stmt tag)
    | DataDef Name [(Name, FreeAnnotation tag)]

data Expr tag
    = LiteralE Literal
    | IdentifierE (Identifier (NameAnnotation tag))
    | FuncExpr [(Name, FreeAnnotation tag)] (EffectType tag) (FreeAnnotation tag) (Stmt tag)
    | Call (Expr tag) [Expr tag]
    | (HasSugar tag) => AccessE (Expr tag) Name
    | (HasSugar tag) => BinE (Binary (Expr tag))
    | (HasSugar tag) => UnaryE (Unary (Expr tag))

data Stmt tag
    = ExprStmt (Expr tag)
    | Block [Stmt tag]
    | While (WhileStmt (Expr tag) (Stmt tag))
    | If (IfStmt (Expr tag) (Stmt tag))
    | Assign (WriteAccess (AnnotatedName (NameAnnotation tag))) (Expr tag)
    | Var Name (FreeAnnotation tag) (Expr tag)
    | Let Name (FreeAnnotation tag) (Expr tag)

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

instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Show (Module stage) where
    show = show . pretty
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Show (TopLevel stage) where
    show = show . pretty
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Show (Stmt stage) where
    show = show . pretty
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Show (Expr stage) where
    show = show . pretty

instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (Module stage) where
    pretty (Program ast) = vcat (pretty <$> ast)
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (TopLevel stage) where
    pretty (FuncDef name sig eff rt body) =
        "fun"
            <+> pretty name
            <>  prettyArgList sig
            <+> "=>"
            <+> pretty eff
            <+> "->"
            <+> pretty rt
            <+> pretty body
    pretty (DataDef name members) =
        "data"
            <+> pretty name
            <+> "{"
            <>  line
            <>  vcat
                    (punctuate
                        ","
                        ((\(n, t) -> pretty n <> ": " <> pretty t) <$> members)
                    )
            <>  line
            <>  "}"
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (Stmt stage) where
    pretty (ExprStmt expr    ) = pretty expr
    pretty (Var name t expr) =
        "var" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (Let name t expr) =
        "let" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (Block stmts) =
        "{" <> line <> indent 4 (vcat (pretty <$> stmts)) <> line <> "}"
    pretty (Assign name expr) = pretty name <+> "=" <+> pretty expr
    pretty (While stmt      ) = pretty stmt
    pretty (If    stmt      ) = pretty stmt
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (Expr stage) where
    pretty (FuncExpr sig eff rt body) =
        "fun"
            <+> prettyArgList sig
            <+> "=>"
            <+> pretty eff
            <+> "->"
            <+> pretty rt
            <+> pretty body
    pretty (LiteralE    lit  ) = pretty lit
    pretty (IdentifierE name ) = pretty name
    pretty (Call callee args ) = pretty callee <> tupled (pretty <$> args)
    pretty (BinE   bin       ) = pretty bin
    pretty (UnaryE unary     ) = pretty unary
    pretty (AccessE expr name) = parens (pretty expr) <> "." <> pretty name

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

deriving instance
    ( AnnotationConstraint Eq (NameAnnotation stage)
    , Eq (AnnotatedName (NameAnnotation stage))
    , Eq (EffectType stage)
    , Eq (FreeAnnotation stage)
    ) => Eq (Module stage)
deriving instance
    ( AnnotationConstraint Eq (NameAnnotation stage)
    , Eq (AnnotatedName (NameAnnotation stage))
    , Eq (EffectType stage)
    , Eq (FreeAnnotation stage)
    ) => Eq (TopLevel stage)
deriving instance
    ( AnnotationConstraint Eq (NameAnnotation stage)
    , Eq (AnnotatedName (NameAnnotation stage))
    , Eq (EffectType stage)
    , Eq (FreeAnnotation stage)
    ) => Eq (Stmt stage)
deriving instance
    ( AnnotationConstraint Eq (NameAnnotation stage)
    , Eq (AnnotatedName (NameAnnotation stage))
    , Eq (EffectType stage)
    , Eq (FreeAnnotation stage)
    ) => Eq (Expr stage)
