module XmlFormatter where

import           Test.Hspec.Formatters

withTestCase :: String -> FormatM () -> FormatM ()
withTestCase name content = do
    writeLine ("<testcase name=\"" <> name <> "\">")
    content
    writeLine "</testcase>"

xmlFormatter :: Formatter
xmlFormatter = Formatter
    { headerFormatter     = do
        writeLine "<?xml version='1.0' encoding='UTF-8'?>"
        writeLine "<testsuites>"

    , exampleGroupStarted = \_ name ->
        writeLine ("<testsuite name=\"" <> name <> "\">")
    , exampleGroupDone    = writeLine "</testsuite>"

    , exampleProgress     = \_ _ -> pure ()
    , exampleSucceeded    = \(_, name) _ -> withTestCase name (pure ())
    , exampleFailed       = \(_, name) _ r -> withTestCase name $ do
        writeLine "<failure>"
        writeLine (show r)
        writeLine "</failure>"
    , examplePending      = \(_, name) _ _ -> withTestCase name $
        writeLine "<skipped/>"

    , failedFormatter     = pure ()
    , footerFormatter     = writeLine "</testsuites>"
    }