module Main where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.Environment
import           Test.Hspec
import           Test.Hspec.Runner
import           Control.Arrow           hiding ( first
                                                , second
                                                )
import           Control.Monad.State
import           Control.Monad.Except

import           Kima.AST
import           Kima.Builtins
import           Kima.Desugar                  as D
import           Kima.Frontend                 as F
import           Kima.Interpreter.Interpreter  as I
import           Kima.Interpreter.Types        as I
import           Kima.Typechecking             as T

import           XmlFormatter
import           Errors

main :: IO ()
main = myHspec $ do
    -- Filenames and contents
    files <- sortBy (\(_, l, _) (_, r, _) -> compare (isJust l) (isJust r))
        <$> runIO (readTestFiles "test/src")


    parallel
        $ context "Full File tests"
        $ forM_ files
        $ \(name, errorSpec, content) -> case errorSpec of
              Just errorTest ->
                  it ("Doesn't run " <> name)
                      $               testForFile name content
                      `shouldSatisfy` \case
                                          Right{}  -> False
                                          Left err -> errorTest err
              Nothing ->
                  it ("Runs " <> name)
                      $               testForFile name content
                      `shouldSatisfy` isRight


testForFile :: String -> String -> Either SomeTestableError ()
testForFile name content =
    testableEither (F.parseProgram name content)
        >>= (pure . D.desugar)
        >>= (testableEither . T.typecheck)
        >>= (testableEither . runInTestInterpreter)

newtype TestInterpreter a = MockInterpreter {
        runInterpreter
                :: StateT (Environment Value) (
                   Either RuntimeError) a
} deriving (Functor, Applicative, Monad, MonadError RuntimeError, MonadState (Environment Value))

runInTestInterpreter :: RuntimeAST 'TopLevel -> Either RuntimeError ()
runInTestInterpreter = (`evalStateT` baseEnv) . runInterpreter . I.runProgram

-- | Always returns "test" on read and ignores output
instance MonadConsole TestInterpreter where
        consoleRead = return "test"
        consoleWrite = const (pure ())

-- Utils

-- | Run a spec while handling some extra arguments
myHspec :: Spec -> IO ()
myHspec spec = do
    args <- getArgs
    let config = if "--junit-output" `elem` args
            then defaultConfig { configFormatter = Just xmlFormatter }
            else defaultConfig
    withArgs (filter (/= "--junit-output") args) $ hspecWith config spec


readTestFiles
    :: FilePath -> IO [(String, Maybe (SomeTestableError -> Bool), String)]
readTestFiles dir = listDirectory dir >>= traverse
    (\path -> do
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

testableEither
    :: (TestableError err, Show err)
    => Either err a
    -> Either SomeTestableError a
testableEither = first SomeTestableError
