module Kima.Test.FileTests (spec) where

import           Test.Hspec
import           Test.Hspec.Core.Spec

import           Data.Maybe
import           Data.Function
import           Data.Foldable
import           Data.List
import           System.FilePath
import           System.Directory.Tree   hiding ( contents )
import           Data.Char
import           Control.Monad.IO.Class

import           Kima.AST
import           Kima.Interface

import           Kima.Test.Interpreters

data TargetStage
    = None
    | Parsing
    | Typechecking

data FileTest = FileTest {
    fileName :: String,
    input :: String,
    isPending :: Bool,
    isFocused :: Bool,
    expectedFailStage :: TargetStage,
    expectedOut :: Maybe String,
    contents :: String
}

spec :: Spec
spec = runIO readTestSources >>= \case
    testDir@Dir{} -> parallel $ context
        "Full File tests"
        (testsForDir (uncurry makeFileTest <$> testDir))
    --  Failed cases
    (File name _) -> it "Failed reading file tests" $ expectationFailure
        ("Found file " <> name <> " where the test directory was expected")
    (Failed _ err) ->
        it "Failed reading file tests" $ expectationFailure (show err)

testsForDir :: DirTree FileTest -> Spec
testsForDir (Dir name contents) =
    context name $ traverse_ testsForDir (sort contents)
testsForDir (File _ contents) = runFileTest contents
testsForDir (Failed name err) =
    xit ("Error " <> show err <> " on " <> name) False

readTestSources :: IO (DirTree (FilePath, String))
readTestSources = do
    (_ :/ testSources) <- readDirectoryWith
        (\path -> (takeFileName path, ) <$> readFile path)
        "test/src"
    return testSources

makeFileTest :: String -> String -> FileTest
makeFileTest name contents =
    let isPending   = not $ null (findPragmas "pending")
        isFocused   = not $ null (findPragmas "focused")
        input       = concat $ findPragmas "input"
        expectedOut = case concat $ findPragmas "output" of
            "" -> Nothing
            s  -> Just s
        shouldParse       = null (findPragmas "shouldNotParse")
        shouldTypecheck   = null (findPragmas "shouldNotTypecheck")
        expectedFailStage = case (shouldParse, shouldTypecheck) of
            (False, _    ) -> Parsing
            (True , False) -> Typechecking
            (True , True ) -> None
    in  FileTest { fileName          = name
                 , input
                 , contents
                 , expectedOut
                 , isPending
                 , isFocused
                 , expectedFailStage
                 }
  where
    findPragmas p =
        lines contents
            & mapMaybe (stripPrefix ("##" <> p))
            & fmap (dropWhile isSpace)
            & fmap
                  (\case
                      ':' : xs -> xs
                      xs       -> xs
                  )
            & fmap (dropWhile isSpace)

runFileTest :: FileTest -> Spec
runFileTest FileTest { fileName, isPending = True } =
    it (fileName ++ " is pending") pending
runFileTest test = maybeFocused $ sequential $ case expectedFailStage test of
    Parsing -> it (fileName test ++ " does not parse")
        $ shouldFail (fromStringTo @Parsed (contents test))

    Typechecking -> it (fileName test ++ " does not typecheck") $ do
        shouldRun (fromStringTo @Parsed (contents test))
        shouldFail (fromStringTo @Typed (contents test))

    None -> it (fileName test ++ " runs") $ shouldRun $ do
        ast <- fromStringTo @Runtime (contents test)
        output <- runModuleWithInput (input test) ast
        liftIO $ expectOutput (expectedOut test) output

    where maybeFocused = if isFocused test then focus else id


instance Show FileTest where
    show FileTest { fileName, isPending } =
        "FileTest { "
            <> "fileName = "
            <> fileName
            <> ", "
            <> "pending = "
            <> show isPending
            <> "}"

    showList xs = (intercalate "\n" (show <$> xs) <>)

instance Eq FileTest where
    (==) FileTest { fileName = name1 } FileTest { fileName = name2 } =
        name1 == name2

instance Ord FileTest where
    compare FileTest { fileName = name1 } FileTest { fileName = name2 } =
        compare name1 name2
