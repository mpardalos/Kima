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
                       | UnexpectedType KType KType
                       | UnavailableType [KType] KType
                       | MismatchedIf KType KType
                       | MissingArgumentTypes
                       | MissingReturnType
                       | MissingFieldTypes
                       | MissingPatternTypes
                       | UnavailableEffect KEffect KEffect
                       | NonExistentEffect Name
                       | NonExistentConstructor KType Name
                       | NonExistentOperation KOperation
                       | NoMatchingFunction [KType]
                       | WrongArgumentTypes [KType]
                       | WrongArgumentCount Int Int
                       | UnexpectedBreak
                       | AmbiguousHandler
    deriving (Eq, Show)

instance Pretty TypecheckingError where
    pretty (UnexpectedType expected got) =
        "Expected" <> line
        <> pretty expected <> line
        <> "but got" <> line
        <> pretty got
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
    pretty (NoMatchingFunction []) = "No matching function for given args."
    pretty (NoMatchingFunction [calleeType]) =
        "Called function of type:" <> line
        <> indent 4 (pretty calleeType) <> line
        <> "But the arguments given did not match the expected argument types"
    pretty (NoMatchingFunction calleeTypes) =
        "Called function has the following overloads:" <> line
        <> indent 4 (bulletList calleeTypes) <> line
        <> "The arguments given did not match any of the possible argument types"
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
    pretty MissingFieldTypes = "Missing field types"
    pretty (UnavailableEffect (KEffect _ available) (KEffect requestedName requested)) =
        "Requested" <+> effectNamePretty <> line
        <> indent 4 (bulletList requested) <> line
        <> availablePretty
        where
            effectNamePretty = case requestedName of
                Just name -> "the effect" <+> pretty name <+> "containing the operations:"
                Nothing -> "the operations:"
            availablePretty = case available of
                [] ->
                    "In a context where no effect is available"
                availableOps ->
                    "In a context where only the following operations are available:" <> line
                    <> indent 4 (bulletList availableOps)
    pretty (NonExistentEffect name) = "This effect does not exist:" <> line <> indent 4 (pretty name)
    pretty (NonExistentOperation op) = "This operation does not exist:" <> line <> indent 4 (pretty op)
    pretty (NonExistentConstructor t cons) = "The type" <+> pretty t <+> "does not have the constructor" <+> pretty cons
    pretty (WrongArgumentTypes argTypes) =
        "Given arguments don't match the expected types:" <> line
        <> indent 4 (tupled (pretty <$> argTypes))
    pretty (WrongArgumentCount expected received) =
        "Expected" <+> pretty expected <+> "arguments, but got" <+> pretty received
    pretty UnexpectedBreak = "Unexpected break statement. It can only be used inside a handler"
    pretty AmbiguousHandler = "Cannot infer type of handle expression"

bulletList :: Pretty a => [a] -> Doc ann
bulletList = vsep . fmap (("•" <+>) . pretty)
