module Kima.Test.FileTests (spec) where

import           Test.Hspec
import           Test.Hspec.Core.Spec

import           Data.Maybe
import           Data.Foldable
import           Data.List
import           Control.Arrow           hiding ( first
                                                , second
                                                )
import           System.FilePath
import           System.Directory.Tree   hiding ( contents )
import           Data.Char

import           Kima.AST
import           Kima.Interface

import           Kima.Test.Interpreters

-- | Up to which stage a test is expected to run
data TargetStage
    = None
    | Parse
    | Desugar
    | Execution

data FileTest = FileTest {
    fileName :: String,
    input :: String,
    isPending :: Bool,
    expectedStage :: TargetStage,
    expectedOut :: Maybe String,
    contents :: String
}



instance Show FileTest where
    show FileTest { fileName, isPending } =
        "FileTest { " <> "fileName = " <> fileName <> ", " <> "pending = " <> show isPending <> "}"

    showList xs = (intercalate "\n" (show <$> xs) <>)

instance Eq FileTest where
    (==) FileTest { fileName = name1 } FileTest { fileName = name2 }
        = (name1 == name2)

instance Ord FileTest where
    compare FileTest { fileName = name1 } FileTest { fileName = name2 }
        = compare name1 name2

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
runFileTest FileTest { fileName, isPending=True} =
    context fileName $ it "Is pending" pending
runFileTest FileTest { fileName, contents, input, expectedStage, expectedOut, isPending=False} =
    sequential $ context fileName $
        case expectedStage of
            None -> doesNotParse
            Parse -> do
                doesParse
                doesNotDesugar
            Desugar -> do
                doesParse
                doesDesugar
                doesNotTypecheck
            Execution -> do
                doesParse
                doesDesugar
                doesTypecheck
                runsCorrectly
    where
        doesParse = it "Parses" $
            shouldRun (fromStringTo @Parsed contents)
        doesDesugar = it "Desugars" $
            shouldRun (fromStringTo @Desugared contents)
        doesTypecheck = it "Typechecks" $
            shouldRun (fromStringTo @Typed contents)
        runsCorrectly = it "Runs" $ do
            ast <- fromStringTo @Runtime contents
            shouldRunWithInputOutput ast input expectedOut

        doesNotParse = it "Does not parse" $
            shouldFail (fromStringTo @Parsed contents)
        doesNotDesugar = it "Does not desugar" $
            shouldFail (fromStringTo @Desugared contents)
        doesNotTypecheck = it "Does not typecheck" $
            shouldFail (fromStringTo @Typed contents)

makeFileTest :: String -> String -> Either String FileTest
makeFileTest name contents = do
    let isPending          = not $ null (findPragmas "pending" contents)
    let input              = concat $ findPragmas "input" contents
    let expectedOut        = case concat $ findPragmas "output" contents of
            "" -> Nothing
            s -> Just s
    let shouldParse     = null (findPragmas "shouldNotParse" contents)
    let shouldDesugar   = null (findPragmas "shouldNotDesugar" contents)
    let shouldTypecheck = null (findPragmas "shouldNotTypecheck" contents)
    let expectedStage = case (shouldParse, shouldDesugar, shouldTypecheck) of
            (False, False, False) -> None
            (True,  False, False) -> Parse
            (True,  True,  False) -> Desugar
            (True,  True,  True ) -> Execution
            (_,     _,     _    ) -> error "Invalid test spec"

    return $ FileTest
        { fileName   = name
        , input
        , contents
        , expectedOut
        , isPending
        , expectedStage
        }
  where
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
