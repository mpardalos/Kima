module Test.FileTests where

import           Test.Hspec

import           Data.Maybe
import           Data.Either
import           Data.List
import           Control.Monad
import           Control.Arrow           hiding ( first
                                                , second
                                                )
import           System.Directory
import           Data.Char
import           Data.Bifunctor

import           Kima.Desugar                  as D
import           Kima.Frontend                 as F
import           Kima.Interpreter              as I
import           Kima.Typechecking             as T

import           Errors
import           Interpreters

fileTestSpec :: Spec
fileTestSpec = do
    -- Filenames and contents
    files <-
        sortBy
                (\(n1, l, _) (n2, r, _) ->
                    compare (isJust l) (isJust r) <> compare n1 n2
                )
            <$> runIO (readTestFiles "test/src")

    parallel $ context "Full File tests" $ forM_ files $ \case
        (name, Just errorTest, content) ->
            it ("Doesn't run " <> name)
                $               testForFile name content
                `shouldSatisfy` \case
                                    Right{}  -> False
                                    Left err -> errorTest err
        (name, Nothing, content) ->
            it ("Runs " <> name)
                $               testForFile name content
                `shouldSatisfy` isRight


testForFile :: String -> String -> Either SomeTestableError Value
testForFile name content =
    testableEither (F.parseProgram name content)
        >>= (pure . D.desugar)
        >>= (testableEither . T.typecheck)
        >>= (testableEither . runInTestInterpreter)

readTestFiles
    :: FilePath -> IO [(String, Maybe (SomeTestableError -> Bool), String)]
readTestFiles dir = listDirectory dir >>= traverse
    (\case
        path -> do
            contents <- readFile (dir <> "/" <> path)
            let expectedErrorNames = findPragmas "shouldFailWith" contents
            return
                ( path
                , if length expectedErrorNames == 0
                    then Nothing
                    else Just (errorMatcherFor expectedErrorNames)
                , contents
                )
    )
  where
    errorMatcherFor = foldl (\f s e -> f e || matchesString s e) (const False)
    -- | Find the values of all occurences of the pragma in a string
    -- | findPragma "hi" "##hi: world" == ["world"]
    findPragmas p =
        lines >>> mapMaybe (stripPrefix ("##" <> p <> ":")) >>> fmap
            (dropWhile isSpace)

-- Pack a testable error in an either into an existential
testableEither
    :: (TestableError err, Show err)
    => Either err a
    -> Either SomeTestableError a
testableEither = first SomeTestableError
