module Kima.Test.FileTests (spec) where

import           Test.Hspec

import           Data.Maybe
import           Data.Foldable
import           Data.List
import           Control.Monad
import           Control.Arrow           hiding ( first
                                                , second
                                                )
import           System.FilePath
import           System.Directory.Tree   hiding ( contents )
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

instance Show FileTest where
    show FileTest { fileName, isPending } =
        "FileTest { " <> "fileName = " <> fileName <> ", " <> "pending = " <> show isPending <> "}"

    showList xs = (intercalate "\n" (show <$> xs) <>)

instance Eq FileTest where
    (==) FileTest { resultSpec = spec1, fileName = name1 } FileTest { resultSpec = spec2, fileName = name2 }
        = (isRight spec1 == isRight spec2) && (name1 == name2)

instance Ord FileTest where
    compare FileTest { resultSpec = spec1, fileName = name1 } FileTest { resultSpec = spec2, fileName = name2 }
        = compare (isLeft spec1) (isLeft spec2) <> compare name1 name2

spec :: Spec
spec = do
    (_ :/ testSources) <- runIO $ readDirectoryWith
        (\fp -> do
            src <- readFile fp
            pure (takeFileName fp, src)
        )
        "test/src"

    case traverse (uncurry makeFileTest) testSources of
        Right (Dir _ contents) ->
            parallel $ context "Full File tests" $ traverse_ dirTreeSpec (sort contents)
        --  Failed cases
        Right (File name _) ->
            it "Failed reading file tests" $ putStrLn (name <> " is not a directory")
        Right (Failed _ err) -> it "Failed reading file tests" $ print err
        Left  err            -> it "Failed reading file tests" $ putStrLn err

dirTreeSpec :: DirTree FileTest -> Spec
dirTreeSpec (Failed name err     ) = xit ("Error " <> show err <> " on " <> name) False
dirTreeSpec (Dir    name contents) = context name (traverse_ dirTreeSpec (sort contents))
dirTreeSpec (File   _    contents) = runFileTest contents

runFileTest :: FileTest -> Spec
runFileTest test@FileTest { fileName, contents, input, resultSpec } = case resultSpec of
    Left errorTest -> it ("Doesn't run " <> fileName) $ if isPending test
        then pending
        else runResult `shouldSatisfy` \case
            Right{}  -> False
            Left err -> errorTest err
    Right outputTest -> it ("Runs " <> fileName) $ if isPending test
        then pending
        else runResult `shouldSatisfy` \case
            Right (_, out) -> outputTest out
            Left{}         -> False
  where
    runResult =
        testableEither (F.parseProgram fileName contents)
            >>= (pure . D.desugar)
            >>= (testableEither . T.typecheck baseTypeCtx)
            >>= (testableEither . runInTestInterpreterWithInput input)

makeFileTest :: String -> String -> Either String FileTest
makeFileTest name contents = do
    let isPending          = not $ null (findPragmas "pending" contents)
    let givenInput         = concat $ findPragmas "input" contents
    let expectedErrorNames = findPragmas "shouldFailWith" contents
    let expectedOut        = concat $ findPragmas "output" contents
    when (not (null expectedErrorNames) && not (null expectedOut))
        $ error ("Test " <> name <> " contains both an expectation for an error and for output")

    return $ FileTest
        { fileName   = name
        , input      = givenInput
        , isPending
        , resultSpec = if not (null expectedOut)
                           then Right (== expectedOut)
                           else if not (null expectedErrorNames)
                               then Left (errorMatcherFor expectedErrorNames)
                               else Right (const True)
        , contents   = contents
        }
  where
    errorMatcherFor = foldl (\f s e -> f e || matchesString s e) (const False)
    -- | Find the values of all occurences of the pragma in a string
    -- | findPragma "hi" "##hi: world" == ["world"]
    findPragmas p =
        lines
            >>> mapMaybe (stripPrefix ("##" <> p))
            >>> fmap (dropWhile isSpace)
            >>> fmap
                    (\case
                        ':' : xs -> xs
                        xs       -> xs
                    )
            >>> fmap (dropWhile isSpace)

-- Pack a testable error in an either into an existential
testableEither :: (TestableError err, Show err) => Either err a -> Either SomeTestableError a
testableEither = first SomeTestableError
