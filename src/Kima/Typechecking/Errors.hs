module Kima.Typechecking.Errors where

import Data.Text.Prettyprint.Doc
import Kima.AST
import Kima.TypeVars
import Kima.Typechecking.Constraints
import Kima.KimaTypes

data TypecheckingError = AmbiguousVariable TypeVar [KType]
                       | AssignToConst (WriteAccess TVarName)
                       | CantUnify TypeVar TypeVar
                       | CantUnifyCall TypeVar [TypeVar]
                       | MultipleSolutions TypeVar [KType]
                       | NameShadowed Name
                       | NoSolution TypeVar
                       | TypeResolutionError TypeExpr
                       | UnexpectedBuiltin BuiltinName
                       | UnboundName TVarIdentifier
                       | NoSuchField (WriteAccess Name) [KType] Name
    deriving (Eq, Show)

instance Pretty TypecheckingError where
    pretty (AmbiguousVariable var types) =
        "Typevar" <+> pretty var <+> "is ambiguous. Available types:"
        <> line
        <> indent 4 (bulletList types)
    pretty (AssignToConst accessor     ) =
        "Assigned to constant" <+> pretty accessor
    pretty (CantUnify l r              ) =
        "Can't unify" <+> pretty l <+> pretty r
    pretty (CantUnifyCall callee args  ) =
        "Can't unify call to" <+> pretty callee
        <+> "with args" <+> tupled (pretty <$> args)
    pretty (MultipleSolutions var sols ) =
        "Typevar" <+> pretty var <+> "has multiple solutions:"
        <> line
        <> indent 4 (bulletList sols)
    pretty (NameShadowed        name ) =
        "Illegal shadowing of" <+> pretty name
    pretty (NoSolution          var  ) =
        "Typevar" <+> pretty var <+> "has no solution"
    pretty (TypeResolutionError expr ) =
        "Can't resolve type" <+> pretty expr
    pretty (UnexpectedBuiltin name) =
        "Got builtin" <+> pretty name <+> "where a normal name was expected"
    pretty (UnboundName         name ) =
        "Reference to unbound name" <+> pretty name
    pretty (NoSuchField accessor baseType requiredField) =
        "In the accessor"
        <> line <>
        indent 4 (pretty accessor)
        <> line <>
        "None of the available types have the field"
        <> line <>
        indent 4 (pretty requiredField)
        <> line <>
        "The available types are:"
        <> line <>
        indent 4 (bulletList baseType)

bulletList :: Pretty a => [a] -> Doc ann
bulletList = vsep . fmap (("•" <+>) . pretty)
