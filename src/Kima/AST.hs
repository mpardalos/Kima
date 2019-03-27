module Kima.AST where

import Control.Applicative
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

    -- Accessors
    Accessor    :: String      -> GenericName 'Nothing b
    TAccessor   :: String -> t -> GenericName ('Just t) b
    
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

-- | A Unified AST for all compilation/interpretation phases.
-- | The type parameters specify which constructors are available.
-- | part : whether this is an expression/statement/some other part of the AST
-- | sugar : Whether the AST contains syntactic sugar. 
-- |         e.g. The Desugarer could be desugar :: AST p 'Sugar n s t -> AST p 'NoSugar n s t
-- | name : The type of names (identifiers) in the AST. E.g. A renamer would be:
-- |        rename :: AST ()
-- | typeann : The type of **user-supplied** type annotations. The typechecker should turn this to **Void**
data AST (part :: ASTPart) (sugar :: Sugar) (name :: Type) (typeAnn :: Maybe Type) where
    Program      :: [AST 'TopLevel sug n t] -> AST 'Module sug n t

    ----------------------- Top-level definitions ----------------------- 
    FuncDef      :: name -> [name]           -> AST 'Stmt sug name 'Nothing  -> AST 'TopLevel sug name 'Nothing
    FuncDefAnn   :: name -> [(name, t)] -> t -> AST 'Stmt sug name ('Just t) -> AST 'TopLevel sug name ('Just t)
    DataDef      :: name -> [name]                                           -> AST 'TopLevel sug name 'Nothing
    DataDefAnn   :: name -> [(name, t)]                                      -> AST 'TopLevel sug name ('Just t)

    ----------------------- Expressions ----------------------- 
    -- Interpreted core
    LiteralE     :: Literal                                          -> AST 'Expr sug    n    t
    Identifier   :: name                                             -> AST 'Expr sug    name t
    FuncExpr     :: [name] -> AST 'Stmt sug name 'Nothing            -> AST 'Expr sug    name 'Nothing
    FuncExprAnn  :: [(name, t)] -> t -> AST 'Stmt sug name ('Just t) -> AST 'Expr sug    name ('Just t)
    Call         :: AST 'Expr sug n t -> [AST 'Expr sug n t]         -> AST 'Expr sug    n    t

    -- Sugar
    BinE         :: Binary (AST 'Expr 'Sugar n t)                    -> AST 'Expr 'Sugar n    t
    UnaryE       :: Unary (AST 'Expr 'Sugar n t)                     -> AST 'Expr 'Sugar n    t

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
-- > Parse ->
type ParsedAST        (p :: ASTPart) = AST p 'Sugar   ParsedName    ('Just TypeExpr)
-- > Desugar ->
type DesugaredAST     (p :: ASTPart) = AST p 'NoSugar DesugaredName ('Just TypeExpr)
-- > Resolve Types ->
type TypeAnnotatedAST (p :: ASTPart) = AST p 'NoSugar DesugaredName ('Just KType)
-- > Typecheck ->
type TypedAST         (p :: ASTPart) = AST p 'NoSugar TypedName     ('Just KType)
-- > Remove type annotations ->
type RuntimeAST       (p :: ASTPart) = AST p 'NoSugar TypedName     'Nothing

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
typeAnnotate :: t -> GenericName a b -> GenericName ( 'Just t) b
typeAnnotate t (Name    n    ) = TypedName n t
typeAnnotate t (Builtin n    ) = TBuiltin n t
typeAnnotate t (TypedName n _) = TypedName n t
typeAnnotate t (TBuiltin  n _) = TBuiltin n t
typeAnnotate t (Accessor n   ) = TAccessor n t
typeAnnotate t (TAccessor n _) = TAccessor n t

deTypeAnnotate :: GenericName ( 'Just t) b -> GenericName 'Nothing b
deTypeAnnotate (TypedName n _) = Name n
deTypeAnnotate (TBuiltin  n _) = Builtin n
deTypeAnnotate (TAccessor n _) = Accessor n

nameType :: GenericName ( 'Just t) b -> t
nameType (TypedName _ t) = t
nameType (TBuiltin  _ t) = t
nameType (TAccessor _ t) = t

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

instance (Pretty n, MaybeConstraint Pretty t) => Show (AST p sug n t) where
    show = show . pretty

instance (Pretty n, MaybeConstraint Pretty t) => Pretty (AST p sug n t) where
    pretty (Program ast) = vcat (pretty <$> ast)
    pretty (FuncDefAnn name sig rt body) = 
        "fun" <+> pretty name <> prettyArgList sig <+> "->" <+> pretty rt <+> pretty body
    pretty (FuncDef name sig body) = 
        "fun" <+> pretty name <> tupled (pretty <$> sig) <+> pretty body
    pretty (FuncExprAnn sig rt body) = 
        "fun" <+> prettyArgList sig <+> "->" <+> pretty rt <+> pretty body
    pretty (FuncExpr sig body) = "fun" <+> tupled (pretty <$> sig) <+> pretty body
    pretty (DataDefAnn name members) = 
        "data" <+> pretty name <+> "{" <> line <>
            vcat (punctuate ","
            ((\(n, t) -> pretty n <> ": " <> pretty t) <$> members))
        <> line <> "}"
    pretty (DataDef name members) =
        "data" <+> pretty name <+> "{" <> line <>
            vcat (punctuate "," (pretty <$> members))
        <> line <> "}"
    pretty (Var name t expr) = "var" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (Let name t expr) = "let" <+> pretty name <> ":" <+> pretty t <+> "=" <+> pretty expr
    pretty (LiteralE lit) = pretty lit
    pretty (Identifier name) = pretty name
    pretty (Call callee args) = pretty callee <> tupled (pretty <$> args)
    pretty (BinE bin) = pretty bin
    pretty (UnaryE unary) = pretty unary
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

instance (MaybeConstraint Show t) => Show (GenericName t b) where
    show (TypedName str t) = "{"  ++ str    ++ " : " ++ show t ++ "}"
    show (TBuiltin n t   ) = "{"  ++ show n ++ " : " ++ show t ++ "}"
    show (TAccessor n t  ) = "{." ++ show n ++ " : " ++ show t ++ "}"
    show (Name str       ) = "{"  ++ str    ++ "}"
    show (Builtin n      ) = "{"  ++ show n ++ "}"
    show (Accessor n     ) = "{." ++ show n ++ "}"

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

instance MaybeConstraint Pretty t => Pretty (GenericName t b) where
    pretty (TypedName str t) = "{"  <> fromString str <+> ":" <+> pretty t <> "}"
    pretty (TBuiltin n t)    = "{"  <> pretty n       <+> ":" <+> pretty t <> "}"
    pretty (TAccessor n t)   = "{." <> pretty n       <+> ":" <+> pretty t <> "}"
    pretty (Name str) = "{" <> fromString str <> "}"
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

instance IsString ParsedName where
    fromString = Name

deriving instance MaybeConstraint Eq t => Eq (GenericName t b)
deriving instance (MaybeConstraint Eq t, MaybeConstraint Ord t) => Ord (GenericName t b) 
deriving instance (MaybeConstraint Eq t, Eq n) => Eq (AST p s n t)

-- Traverals

mapTypeAnnotations :: forall n t1 t2 p sug. (t1 -> t2) -> AST p sug n ('Just t1) -> AST p sug n ('Just t2)
mapTypeAnnotations = coerce (traverseTypeAnnotations :: (t1 -> Identity t2) -> AST p sug n ('Just t1) -> Identity (AST p sug n ('Just t2)))

traverseTypeAnnotations :: Applicative m => (t1 -> m t2) -> AST p sug n ('Just t1) -> m (AST p sug n ('Just t2))
traverseTypeAnnotations f (Program ast)            = Program <$> traverse (traverseTypeAnnotations f) ast
traverseTypeAnnotations f (DataDefAnn n members)   = DataDefAnn n <$> traverse (traverse f) members
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
removeTypeAnnotations (DataDefAnn n members)  = DataDef n (fst <$> members)
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
traverseNames f (DataDef n members)      = DataDef     <$> f n <*> traverse f members
traverseNames f (DataDefAnn n members)   = DataDefAnn  <$> f n <*> traverse (traverseTuple1 f) members
traverseNames f (Call callee args)       = Call        <$> traverseNames f callee                               <*> traverse (traverseNames f) args
traverseNames f (Assign n e)             = Assign <$> f n            <*> traverseNames f e
traverseNames f (Var n t e)              = Var    <$> f n <*> pure t <*> traverseNames f e
traverseNames f (Let n t e)              = Let    <$> f n <*> pure t <*> traverseNames f e

traverseTuple1 :: Applicative m => (a -> m c) -> (a, b) -> m (c, b)
traverseTuple1 f (a, b) = liftA2 (,) (f a) (pure b)

-- Patterns
{-# COMPLETE StmtAST, ExprAST, ProgramAST, TopLevelAST#-}

pattern StmtAST :: AST 'Stmt s n t -> AST p s n t
pattern StmtAST stmt <- (isStmt -> Just stmt)

isStmt :: AST p s n t -> Maybe (AST 'Stmt s n t)
isStmt stmt@Assign{}   = Just stmt
isStmt BinE{}          = Nothing
isStmt stmt@Block{}    = Just stmt
isStmt Call{}          = Nothing
isStmt DataDef{}       = Nothing
isStmt DataDefAnn{}    = Nothing
isStmt stmt@ExprStmt{} = Just stmt
isStmt FuncDef{}       = Nothing
isStmt FuncDefAnn{}    = Nothing
isStmt FuncExpr{}      = Nothing
isStmt FuncExprAnn{}   = Nothing
isStmt Identifier{}    = Nothing
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
isExpr DataDef{}          = Nothing
isExpr DataDefAnn{}       = Nothing
isExpr ExprStmt{}         = Nothing
isExpr FuncDef{}          = Nothing
isExpr FuncDefAnn{}       = Nothing
isExpr expr@FuncExpr{}    = Just expr
isExpr expr@FuncExprAnn{} = Just expr
isExpr expr@Identifier{}  = Just expr
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
isProgram DataDef{}     = Nothing
isProgram DataDefAnn{}  = Nothing
isProgram ExprStmt{}    = Nothing
isProgram FuncDef{}     = Nothing
isProgram FuncDefAnn{}  = Nothing
isProgram FuncExpr{}    = Nothing
isProgram FuncExprAnn{} = Nothing
isProgram Identifier{}  = Nothing
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
isTopLevelAST ast@DataDef{}    = Just ast
isTopLevelAST ast@DataDefAnn{} = Just ast
isTopLevelAST ExprStmt{}       = Nothing
isTopLevelAST ast@FuncDef{}    = Just ast
isTopLevelAST ast@FuncDefAnn{} = Just ast
isTopLevelAST FuncExpr{}       = Nothing
isTopLevelAST FuncExprAnn{}    = Nothing
isTopLevelAST Identifier{}     = Nothing
isTopLevelAST If{}             = Nothing
isTopLevelAST Let{}            = Nothing
isTopLevelAST LiteralE{}       = Nothing
isTopLevelAST Program{}        = Nothing
isTopLevelAST UnaryE{}         = Nothing
isTopLevelAST Var{}            = Nothing
isTopLevelAST While{}          = Nothing

-- Utils 

type family MaybeConstraint (f :: k -> Constraint) (x :: Maybe k) :: Constraint where
    MaybeConstraint f 'Nothing = ()
    MaybeConstraint f ('Just a) = f a
