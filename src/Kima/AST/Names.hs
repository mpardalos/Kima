module Kima.AST.Names where

import Data.Kind
import Data.String
import Data.Text.Prettyprint.Doc
import GHC.Generics

import Kima.AST.Kinds

data BuiltinName = AddOp | SubOp | MulOp | ModOp | DivOp  -- Binary ops
                 | GTOp | GTEOp | LTOp | LTEOp | EqualsOp
                 | PowOp
                 | InvertOp | NegateOp -- Unary ops
    deriving (Show, Eq, Ord, Generic)

type Name = String

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

data AnnotatedName :: HasAnnotation -> Type where
    Name  :: String      -> AnnotatedName 'NoAnnotation
    TName :: String -> t -> AnnotatedName ('Annotation t)


-------------------- Deriveable instances -----------------------------
deriving instance AnnotationConstraint Eq t => Eq (Identifier t)
deriving instance AnnotationConstraint Eq t => Eq (AnnotatedName t)
deriving instance (AnnotationConstraint Eq t,
                   AnnotationConstraint Ord t) =>
                  Ord (Identifier t)

-------------------- Show ---------------------------------------------
instance (AnnotationConstraint Show t) => Show (Identifier t) where
    show (TIdentifier n t) = "{" ++ n ++ " : " ++ show t ++ "}"
    show (TBuiltin    n t) = "{" ++ show n ++ " : " ++ show t ++ "}"
    show (TAccessor   n t) = "{." ++ show n ++ " : " ++ show t ++ "}"
    show (Identifier str ) = "{" ++ str ++ "}"
    show (Builtin    n   ) = "{" ++ show n ++ "}"
    show (Accessor   str ) = "{." ++ str ++ "}"

instance (AnnotationConstraint Show t) => Show (AnnotatedName t) where
    show (TName n t) = "{" ++ n ++ " : " ++ show t ++ "}"
    show (Name str ) = "{" ++ str ++ "}"

-------------------- Pretty ---------------------------------------------
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
    pretty PowOp     = "(**)"

-- | Prints a format that doesn't match the source, i.e. it doesn't parse.
-- | Maybe it should be changed
instance AnnotationConstraint Pretty t => Pretty (AnnotatedName t) where
    pretty (TName str t) = "{" <> fromString str <+> ":" <+> pretty t <> "}"
    pretty (Name str   ) = "{" <> fromString str <> "}"

-- | Prints a format that doesn't match the source, i.e. it doesn't parse.
-- | Maybe it should be changed
instance AnnotationConstraint Pretty t => Pretty (Identifier t) where
    pretty (TIdentifier str t) =
        "{" <> fromString str <> ":" <+> pretty t <> "}"
    pretty (TBuiltin  n t ) = "{" <> pretty n <+> ":" <+> pretty t <> "}"
    pretty (TAccessor n t ) = "{." <> pretty n <+> ":" <+> pretty t <> "}"
    pretty (Identifier str) = "{" <> fromString str <> "}"
    pretty (Builtin    n  ) = "{" <> pretty n <> "}"
    pretty (Accessor   n  ) = "{." <> fromString n <> "}"

instance IsString (Identifier 'NoAnnotation) where
    fromString ('.' : name) = Accessor name
    fromString name         = Identifier name

instance IsString (AnnotatedName 'NoAnnotation) where
    fromString = Name

-------------------- Traversals --------------------------------------------
traverseAnnotation
    :: Functor m
    => (t1 -> m t2)
    -> Identifier ( 'Annotation t1)
    -> m (Identifier ( 'Annotation t2))
traverseAnnotation f (TIdentifier n t) = TIdentifier n <$> f t
traverseAnnotation f (TBuiltin    n t) = TBuiltin n <$> f t
traverseAnnotation f (TAccessor   n t) = TAccessor n <$> f t

--------- Useful functions ----------

class IdentifierLike ident where
    typeAnnotate :: t -> ident a -> ident ('Annotation t)
    deTypeAnnotate :: ident ('Annotation t) -> ident 'NoAnnotation
    nameType :: ident ('Annotation t) -> t

    toIdentifier :: ident a -> Identifier a

instance IdentifierLike AnnotatedName where
    toIdentifier (Name n   ) = Identifier n
    toIdentifier (TName n t) = TIdentifier n t

    typeAnnotate t (Name n   ) = TName n t
    typeAnnotate t (TName n _) = TName n t

    deTypeAnnotate (TName n _) = Name n

    nameType (TName _ t) = t

instance IdentifierLike Identifier where
    toIdentifier = id

    typeAnnotate t (Identifier n   ) = TIdentifier n t
    typeAnnotate t (TIdentifier n _) = TIdentifier n t
    typeAnnotate t (Builtin n      ) = TBuiltin n t
    typeAnnotate t (TBuiltin n _   ) = TBuiltin n t
    typeAnnotate t (Accessor n     ) = TAccessor n t
    typeAnnotate t (TAccessor n _  ) = TAccessor n t

    deTypeAnnotate (TIdentifier n _) = Identifier n
    deTypeAnnotate (TBuiltin    n _) = Builtin n
    deTypeAnnotate (TAccessor   n _) = Accessor n

    nameType (TIdentifier _ t) = t
    nameType (TBuiltin    _ t) = t
    nameType (TAccessor   _ t) = t
