module Kima.Typechecking.Errors (TypecheckingError(..)) where

import           Data.Text.Prettyprint.Doc
import           Kima.AST
import           Kima.KimaTypes

data TypecheckingError = AssignToConst (WriteAccess (AnnotatedName 'NoAnnotation))
                       | NameShadowed Name
                       | TypeResolutionError TypeExpr
                       | UnexpectedBuiltin BuiltinName
                       | UnboundName (Identifier 'NoAnnotation)
                       | NoSuchField (WriteAccess Name) [KType] Name
    deriving (Eq, Show)

instance Pretty TypecheckingError where
    pretty (AssignToConst accessor     ) =
        "Assigned to constant" <+> pretty accessor
    pretty (NameShadowed        name ) =
        "Illegal shadowing of" <+> pretty name
    pretty (TypeResolutionError expr ) =
        "Can't resolve type" <+> pretty expr
    pretty (UnexpectedBuiltin name) =
        "Got builtin" <+> pretty name <+> "where a normal name was expected"
    pretty (UnboundName name) = "Reference to unbound name" <+> pretty name
    pretty (NoSuchField accessor baseType requiredField) =
        "In the accessor"
            <> line
            <> indent 4 (pretty accessor)
            <> line
            <> "None of the available types have the field"
            <> line
            <> indent 4 (pretty requiredField)
            <> line
            <> "The available types are:"
            <> line
            <> indent 4 (bulletList baseType)

bulletList :: Pretty a => [a] -> Doc ann
bulletList = vsep . fmap (("•" <+>) . pretty)
