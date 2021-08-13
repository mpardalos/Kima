module Kima.AST.AST where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Text.Prettyprint.Doc

import GHC.Generics

import Kima.AST.Kinds
import Kima.AST.Names

newtype Module tag = Module [TopLevel tag]

data TopLevel tag
    = FuncDef Name [(Name, FreeAnnotation tag)] (EffectType tag) (FreeAnnotation tag) (Stmt tag)
    | ProductTypeDef Name [(Name, FreeAnnotation tag)]
    | SumTypeDef Name [(Name, [FreeAnnotation tag])]
    | OperationDef Name [(Name, FreeAnnotation tag)] (FreeAnnotation tag)
    | EffectSynonymDef Name [Name]

data Expr tag
    = LiteralExpr Literal
    | IdentifierExpr (Identifier (NameAnnotation tag))
    | FuncExpr [(Name, FreeAnnotation tag)] (EffectType tag) (FreeAnnotation tag) (Stmt tag)
    | CallExpr (Expr tag) [Expr tag]
    | HandleExpr (Stmt tag) [HandlerClause tag]
    | MatchExpr (Expr tag) [MatchClause tag]
    | (HasSugar tag) => SimpleHandleExpr (Expr tag) [HandlerClause tag]
    | (HasSugar tag) => AccessExpr (Expr tag) Name
    | (HasSugar tag) => BinExpr BinaryOp (Expr tag) (Expr tag)
    | (HasSugar tag) => UnaryExpr UnaryOp (Expr tag)

data Stmt tag
    = ExprStmt (Expr tag)
    | BlockStmt [Stmt tag]
    | WhileStmt (While (Expr tag) (Stmt tag))
    | IfStmt (If (Expr tag) (Stmt tag))
    | AssignStmt (WriteAccess (AnnotatedName (NameAnnotation tag))) (Expr tag)
    | VarStmt Name (FreeAnnotation tag) (Expr tag)
    | LetStmt Name (FreeAnnotation tag) (Expr tag)
    | BreakStmt (Expr tag)
    | HasSugar tag => SimpleBreakStmt
    | HasSugar tag => SimpleIfStmt (Expr tag) (Stmt tag)

data Pattern tag
    = ConstructorPattern Name [Pattern tag]
    | WildcardPattern Name (FreeAnnotation tag)

data MatchClause tag = MatchClause
    { clausePattern :: Pattern tag
    , clauseBody :: Stmt tag
    }

data HandlerClause tag = HandlerClause
    { opName :: Name
    , args :: [(Name, FreeAnnotation tag)]
    , returnType :: FreeAnnotation tag
    , body :: Stmt tag
    }

---------------- Factored out parts of the AST ------------------------------

data Literal
    = IntLit Integer
    | FloatLit Double
    | BoolLit Bool
    | StringLit String
    | UnitLit
    deriving (Eq, Generic)

data If cond body = If {
    cond :: cond,
    ifBlk :: body,
    elseBlk :: body
} deriving (Eq, Generic)

data While cond body = While {
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

instance (Pretty cond, Pretty stmt) => Pretty (If cond stmt) where
    pretty If { cond, ifBlk, elseBlk } =
        "if"
            <+> parens (pretty cond)
            <+> pretty ifBlk
            <+> "else"
            <+> pretty elseBlk

instance (Pretty cond, Pretty stmt) => Pretty (While cond stmt) where
    pretty While { cond, body } =
        "while (" <+> pretty cond <+> ") " <+> pretty body

instance Show Literal where
    show = show . pretty

instance Pretty Literal where
    pretty (IntLit    n) = "i" <> pretty n
    pretty (FloatLit  f) = "f" <> pretty f
    pretty (BoolLit   b) = pretty b
    pretty (StringLit s) = "s" <> "\"" <> pretty s <> "\""
    pretty UnitLit       = "()"

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
    ) => Show (HandlerClause stage) where
    show = show . pretty
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Show (Pattern stage) where
    show = show . pretty

instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (Module stage) where
    pretty (Module ast) = vcat (pretty <$> ast)
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
            <+> ":"
            <+> pretty eff
            <+> "->"
            <+> pretty rt
            <+> pretty body
    pretty (ProductTypeDef name members) =
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
    pretty (OperationDef name args rt) =
        "effect" <+> pretty name <> prettyArgList args <+> "->" <+> pretty rt
    pretty (EffectSynonymDef name ops) =
        "effect"
            <+> pretty name
            <>  "{"
            <>  line
            <>  vcat (punctuate comma (pretty <$> ops))
            <>  line
            <>  "}"
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (Stmt stage) where
    pretty (ExprStmt expr) = pretty expr
    pretty (VarStmt name t expr) =
        "var" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (LetStmt name t expr) =
        "let" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (BlockStmt stmts) =
        "{" <> line <> indent 4 (vcat (pretty <$> stmts)) <> line <> "}"
    pretty (AssignStmt name expr  ) = pretty name <+> "=" <+> pretty expr
    pretty (WhileStmt stmt        ) = pretty stmt
    pretty (SimpleIfStmt cond body) = "if" <+> parens (pretty cond) <+> pretty body
    pretty (IfStmt stmt           ) = pretty stmt
    pretty SimpleBreakStmt          = "break"
    pretty (BreakStmt expr        ) = "break" <+> pretty expr
instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (Expr stage) where
    pretty (FuncExpr sig eff rt body) =
        "fun"
            <+> prettyArgList sig
            <+> ":"
            <+> pretty eff
            <+> "->"
            <+> pretty rt
            <+> pretty body
    pretty (LiteralExpr    lit ) = pretty lit
    pretty (IdentifierExpr name) = pretty name
    pretty (CallExpr callee args) = pretty callee <> tupled (pretty <$> args)
    pretty (SimpleHandleExpr expr handlers) =
        "handle"
            <+> pretty expr
            <+> "{"
            <>  line
            <>  indent 4 (vcat (pretty <$> handlers))
            <>  line
            <>  "}"
    pretty (HandleExpr stmt handlers) =
        "handle" <+> pretty stmt
            <+> "with {"
            <>  line
            <>  indent 4 (vcat (pretty <$> handlers))
            <>  line
            <>  "}"
    pretty (BinExpr op l r      ) = pretty l <+> pretty op <+> pretty r
    pretty (UnaryExpr  op   e   ) = pretty op <> pretty e
    pretty (AccessExpr expr name) = parens (pretty expr) <> "." <> pretty name

instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (HandlerClause stage) where
    pretty (HandlerClause name args rt body) =
        pretty name <> prettyArgList args <+> "->" <+> pretty rt <+> pretty body

instance
    ( AnnotationConstraint Pretty (NameAnnotation stage)
    , Pretty (AnnotatedName (NameAnnotation stage))
    , Pretty (EffectType stage)
    , Pretty (FreeAnnotation stage)
    ) => Pretty (Pattern stage) where
    pretty (WildcardPattern name _t) = pretty name
    pretty (ConstructorPattern constructor args) = pretty constructor <+> tupled (pretty <$> args)

instance Pretty ident => Pretty (WriteAccess ident) where
    pretty (WriteAccess ident rest) =
        hcat $ punctuate "." (pretty <$> ident : rest)

instance Pretty ident => Show (WriteAccess ident) where
    show = show . pretty

--------------- Boring instances ---------------------

instance Bifunctor If where
    bimap f g If { cond, ifBlk, elseBlk } =
        If (f cond) (g ifBlk) (g elseBlk)

instance Bifoldable If where
    bifoldMap f g If { cond, ifBlk, elseBlk } =
        f cond <> g ifBlk <> g elseBlk

instance Bitraversable If where
    bitraverse f g If { cond, ifBlk, elseBlk } =
        (\(a, b, c) -> If a b c)
            <$> ((,,) <$> f cond <*> g ifBlk <*> g elseBlk)

instance Bifunctor While where
    bimap f g While { cond, body } = While (f cond) (g body)

instance Bifoldable While where
    bifoldMap f g While { cond, body } = f cond <> g body

instance Bitraversable While where
    bitraverse f g While { cond, body } =
        uncurry While <$> bitraverse f g (cond, body)

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
deriving instance
    ( AnnotationConstraint Eq (NameAnnotation stage)
    , Eq (AnnotatedName (NameAnnotation stage))
    , Eq (EffectType stage)
    , Eq (FreeAnnotation stage)
    ) => Eq (HandlerClause stage)
deriving instance
    ( AnnotationConstraint Eq (NameAnnotation stage)
    , Eq (AnnotatedName (NameAnnotation stage))
    , Eq (EffectType stage)
    , Eq (FreeAnnotation stage)
    ) => Eq (Pattern stage)
deriving instance
    ( AnnotationConstraint Eq (NameAnnotation stage)
    , Eq (AnnotatedName (NameAnnotation stage))
    , Eq (EffectType stage)
    , Eq (FreeAnnotation stage)
    ) => Eq (MatchClause stage)
