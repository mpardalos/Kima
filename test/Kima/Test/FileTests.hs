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
            parallel $ context "Full File tests" $ traverse_ dirTreeSpec
                                                             (sort contents)
        --  Failed cases
        Right (File name _) -> it "Failed reading file tests"
            $ putStrLn (name <> " is not a directory")
        Right (Failed _ err) -> it "Failed reading file tests" $ print err
        Left  err            -> it "Failed reading file tests" $ putStrLn err

dirTreeSpec :: DirTree FileTest -> Spec
dirTreeSpec (Failed name err) =
    xit ("Error " <> show err <> " on " <> name) False
dirTreeSpec (Dir name contents) =
    context name (traverse_ dirTreeSpec (sort contents))
dirTreeSpec (File _ contents) = runFileTest contents


runFileTest :: FileTest -> Spec
runFileTest FileTest { fileName, isPending = True } =
    it (fileName ++ " is pending") pending
runFileTest test = maybeFocused $ sequential $ case expectedFailStage test of
    Parsing -> it (fileName test ++ " does not parse")
        $ shouldFail (fromStringTo @Parsed (contents test))

    Typechecking -> it (fileName test ++ " does not typecheck") $ do
        shouldRun (fromStringTo @Parsed (contents test))
        shouldFail (fromStringTo @Typed (contents test))

    None -> it (fileName test ++ " runs") $ do
        ast <- fromStringTo @Runtime (contents test)
        shouldRunWithInputOutput ast (input test) (expectedOut test)
    where maybeFocused = if isFocused test then focus else id

makeFileTest :: String -> String -> Either String FileTest
makeFileTest name contents = do
    let isPending = not $ null (findPragmas "pending" contents)
    let isFocused = not $ null (findPragmas "focused" contents)
    let input     = concat $ findPragmas "input" contents
    let expectedOut = case concat $ findPragmas "output" contents of
            "" -> Nothing
            s  -> Just s
    let shouldParse     = null (findPragmas "shouldNotParse" contents)
    let shouldTypecheck = null (findPragmas "shouldNotTypecheck" contents)
    let expectedFailStage = case (shouldParse, shouldTypecheck) of
            (False, False) -> Parsing
            (True , False) -> Typechecking
            (True , True ) -> None
            (False, True ) -> error "Invalid test spec"

    return $ FileTest { fileName          = name
                      , input
                      , contents
                      , expectedOut
                      , isPending
                      , isFocused
                      , expectedFailStage
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
