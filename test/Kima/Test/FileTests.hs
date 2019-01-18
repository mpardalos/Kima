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
import           Data.Either
import           Data.Bifunctor

import           Kima.Builtins                 as B
import           Kima.Desugar                  as D
import           Kima.Frontend                 as F
import           Kima.Typechecking             as T

import           Kima.Test.Errors
import           Kima.Test.Interpreters

data FileTest = FileTest {
    fileName :: String,
    input :: String,
    isPending :: Bool,
    -- | Left -> expect an error fullfiling that spec
    -- | Right -> expect output fullfiling that spec
    resultSpec :: Either (SomeTestableError -> Bool) (String -> Bool),
    contents :: String
}

(<&>) = flip fmap

spec :: Spec
spec = parallel $ do
    -- Filenames and contents
    files <- runIO (readTestFiles "test/src")
        <&> sortBy (
            \FileTest { resultSpec = spec1, fileName = name1 } 
             FileTest { resultSpec = spec2, fileName = name2 } ->
                compare (isRight spec1) (isRight spec2) <> compare name1 name2
            )

    parallel $ context "Full File tests" $ forM_ files runFileTest 


runFileTest :: FileTest -> Spec
runFileTest test@FileTest { fileName, contents, input } = case test of
    FileTest { resultSpec = Left errorTest } ->
        it ("Doesn't run " <> fileName) $ 
            if isPending test then pending else
                runResult `shouldSatisfy` \case
                    Right{}  -> False
                    Left err -> errorTest err
    FileTest { resultSpec = Right outputTest } ->
        it ("Runs " <> fileName) $
            if isPending test then pending else
                runResult `shouldSatisfy` \case
                    Right (_, out) -> outputTest out
                    Left{}    -> False
    where
        runResult = testableEither (F.parseProgram fileName contents)
            >>= (pure . D.desugar)
            >>= (testableEither . T.typecheck baseTypeCtx)
            >>= (testableEither . runInTestInterpreterWithInput input)

readTestFiles
    :: FilePath -> IO [FileTest]
readTestFiles dir = listDirectory dir >>= traverse (\path -> do
    contents <- readFile (dir <> "/" <> path)
    let isPending          = not    $ null (findPragmas "pending" contents)
    let givenInput         = concat $ findPragmas "input" contents
    let expectedErrorNames = findPragmas "shouldFailWith" contents
    let expectedOut        = concat $ findPragmas "output" contents
    when (not (null expectedErrorNames) && not (null expectedOut)) $
        error ("Test " <> path <> " contains both an expectation for an error and for output")

    return $ FileTest 
        { fileName   = path
        , input      = givenInput
        , isPending
        , resultSpec = if not (null expectedOut)
            then Right (==expectedOut)
            else (if not (null expectedErrorNames)
                then Left (errorMatcherFor expectedErrorNames)
                else Right (const True))
        , contents   = contents
    })
  where
    errorMatcherFor = foldl (\f s e -> f e || matchesString s e) (const False)
    -- | Find the values of all occurences of the pragma in a string
    -- | findPragma "hi" "##hi: world" == ["world"]
    findPragmas p =
        lines >>> mapMaybe (stripPrefix ("##" <> p))
            >>> fmap (dropWhile isSpace)
            >>> fmap (\case
                ':':xs -> xs
                xs     -> xs
            ) 
            >>> fmap (dropWhile isSpace)

-- Pack a testable error in an either into an existential
testableEither
    :: (TestableError err, Show err)
    => Either err a
    -> Either SomeTestableError a
testableEither = first SomeTestableError
