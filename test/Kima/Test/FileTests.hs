module Kima.Test.FileTests where

import           Test.Hspec

import           Data.Maybe
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
import           Kima.Typechecking             as T

import           Kima.Test.Errors
import           Kima.Test.Interpreters

data FileTest = FileTest {
    fileName :: String,
    input :: String,
    outputSpec :: Maybe String,
    errorSpec :: Maybe (SomeTestableError -> Bool),
    contents :: String
}

spec :: Spec
spec = parallel $ do
    -- Filenames and contents
    files <-
        sortBy
                (\FileTest { errorSpec = spec1, fileName = name1 } 
                  FileTest { errorSpec = spec2, fileName = name2 } ->
                    compare (isJust spec1) (isJust spec2) <> compare name1 name2
                )
            <$> runIO (readTestFiles "test/src")

    parallel $ context "Full File tests" $ forM_ files $ \case
        test@FileTest { errorSpec = Just errorTest } ->
            it ("Doesn't run " <> fileName test)
                $               runFileTest test
                `shouldSatisfy` \case
                    Right{}  -> False
                    Left err -> errorTest err
        test@FileTest { errorSpec = Nothing, outputSpec } ->
            it ("Runs " <> fileName test)
                $               runFileTest test
                `shouldSatisfy` \case
                    Left{}       -> False
                    Right output -> case outputSpec of
                        Just expectedOut -> output == expectedOut
                        Nothing -> True


runFileTest :: FileTest -> Either SomeTestableError String
runFileTest FileTest { fileName, contents, input } =
    testableEither (F.parseProgram fileName contents)
        >>= (pure . D.desugar)
        >>= (testableEither . T.typecheck)
        >>= (testableEither . runInTestInterpreterWithInput input)
        >>= (pure . snd)

readTestFiles
    :: FilePath -> IO [FileTest]
readTestFiles dir = listDirectory dir >>= traverse
    (\case
        path -> do
            contents <- readFile (dir <> "/" <> path)
            let expectedErrorNames = findPragmas "shouldFailWith" contents
            let givenInput         = concat $ findPragmas "input" contents
            let expectedOut        = concat $ findPragmas "output" contents
            return $ FileTest 
                { fileName = path
                , input = givenInput
                , outputSpec = if null expectedOut
                    then Nothing
                    else Just expectedOut
                , errorSpec = if null expectedErrorNames
                   then Nothing
                   else Just (errorMatcherFor expectedErrorNames)
                , contents = contents
            }
                
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
