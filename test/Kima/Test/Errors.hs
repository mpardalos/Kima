module Kima.Test.Errors where

import           Kima.Interpreter               ( RuntimeError(..) )
import           Kima.Typechecking              ( TypecheckingError(..) )
import           Text.Megaparsec

data SomeTestableError = forall err. TestableError err => SomeTestableError err

class TestableError err where
    matchesString :: String -> err -> Bool
    showError :: err -> String

    default showError :: Show err => err -> String
    showError = show

instance (Stream a, ShowErrorComponent b) => TestableError (ParseError a b) where
    matchesString "ParseError" _ = True
    matchesString _ _            = False
    showError                    = parseErrorPretty

instance (Stream a, ShowErrorComponent b) => TestableError (ParseErrorBundle a b) where
    matchesString "ParseError" _ = True
    matchesString _ _            = False
    showError                    = errorBundlePretty

instance TestableError RuntimeError where
    matchesString "NotInScope"           NotInScope{}           = True
    matchesString "WrongArgumentCount"   WrongArgumentCount{}   = True
    matchesString "WrongConditionType"   WrongConditionType{}   = True
    matchesString "NotAFunction"         NotAFunction{}         = True
    matchesString "BuiltinFunctionError" BuiltinFunctionError{} = True
    matchesString _ _ = False


instance TestableError TypecheckingError where
    matchesString "AmbiguousVariable"   AmbiguousVariable{}   = True
    matchesString "CantUnify"           CantUnify{}           = True
    matchesString "CantUnifyCall"       CantUnifyCall{}       = True
    matchesString "DomainMismatch"      DomainMismatch        = True
    matchesString "MultipleSolutions"   MultipleSolutions{}   = True
    matchesString "NameShadowed"        NameShadowed{}        = True
    matchesString "NoSolution"          NoSolution{}          = True
    matchesString "TypeResolutionError" TypeResolutionError{} = True
    matchesString "UnboundName"         UnboundName{}         = True
    matchesString "UnsetDomain"         UnsetDomain{}         = True
    matchesString "AssignToConst"       AssignToConst{}       = True
    matchesString _ _ = False

instance TestableError SomeTestableError where
    matchesString s (SomeTestableError err) = matchesString s err

instance Show SomeTestableError where
    show (SomeTestableError err) = showError err
