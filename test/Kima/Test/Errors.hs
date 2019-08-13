module Kima.Test.Errors where

import           Kima.Interpreter               ( RuntimeError(..) )
import           Kima.Typechecking              ( TypecheckingError(..) )
import           Text.Megaparsec
import           Data.Text.Prettyprint.Doc

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

    showError = ("Runtime error: " <>) . show . pretty


instance TestableError TypecheckingError where
    matchesString "NameShadowed"        NameShadowed{}        = True
    matchesString "TypeResolutionError" TypeResolutionError{} = True
    matchesString "UnboundName"         UnboundName{}         = True
    matchesString "AssignToConst"       AssignToConst{}       = True
    matchesString _ _ = False
    showError = ("Typechecking error: " <>) . show . pretty

instance TestableError SomeTestableError where
    matchesString s (SomeTestableError err) = matchesString s err

instance Show SomeTestableError where
    show (SomeTestableError err) = showError err
