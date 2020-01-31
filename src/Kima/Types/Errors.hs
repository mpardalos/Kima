module Kima.Types.Errors (TypecheckingError(..)) where

import           Data.Text.Prettyprint.Doc
import           Kima.AST

data TypecheckingError = AssignToConst (WriteAccess (AnnotatedName 'NoAnnotation))
                       | AmbiguousName (Identifier 'NoAnnotation) [KType]
                       | NameShadowed Name
                       | TypeResolutionError TypeExpr
                       | UnboundName (Identifier 'NoAnnotation)
                       | NoSuchField KType Name
                       | AmbiguousCall [KType]
                       | NoMatchingFunction
                       | UnexpectedType KType KType
                       | UnavailableType [KType] KType
                       | MismatchedIf KType KType
                       | MissingArgumentTypes
                       | MissingEffectType
                       | MissingReturnType
                       | MissingFieldTypes
    deriving (Eq, Show)

instance Pretty TypecheckingError where
    pretty (UnexpectedType expected got) =
        "Expected" <+> pretty expected <+> "but got" <+> pretty got
    pretty (UnavailableType [available] requested) = pretty (UnexpectedType requested available)
    pretty (UnavailableType available requested) =
        "Expected" <+> pretty requested <+> "but got one of" <> line <> indent
            4
            (bulletList available)
    pretty (AmbiguousName name types) =
        "Ambiguous type for"
            <+> pretty name
            <+> "possibilities are:"
            <>  line
            <>  indent 4 (bulletList types)
    pretty (AmbiguousCall calleeTypes) = "Ambiguous call. Possible callee types are:"
        <> line
        <> indent 4 (bulletList calleeTypes)
    pretty NoMatchingFunction          = "No matching function for given args"
    pretty (AssignToConst accessor) =
        "Assigned to constant" <+> pretty accessor
    pretty (NameShadowed        name) = "Illegal shadowing of" <+> pretty name
    pretty (TypeResolutionError expr) = "Can't resolve type" <+> pretty expr
    pretty (UnboundName name) = "Reference to unbound name" <+> pretty name
    pretty (NoSuchField recordType requiredField) =
        "The type" <+> pretty recordType <+> "does not have field" <> pretty requiredField
    pretty (MismatchedIf thenBlkType elseBlkType) =
        "\"then\" block returns" <+> pretty thenBlkType
        <+> "but \"else\" block returns" <> pretty elseBlkType
    pretty MissingArgumentTypes = "Missing argument types"
    pretty MissingReturnType = "Missing return type"
    pretty MissingEffectType = "Missing effect type"
    pretty MissingFieldTypes = "Missing field types"

bulletList :: Pretty a => [a] -> Doc ann
bulletList = vsep . fmap (("•" <+>) . pretty)
