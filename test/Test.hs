module Main where

import           Test.Hspec
import           Test.Hspec.Runner
import           Control.Monad
import           System.Directory
import           System.Environment
import           Data.Maybe
import           Control.Monad.State
import           Control.Monad.Except

import           Kima.AST
import           Kima.Frontend                 as F
import           Kima.Desugar                  as D
import           Kima.Typechecking             as T
import           Kima.Interpreter.Types        as I
import           Kima.Interpreter.Interpreter  as I
import           Kima.Interpreter.Builtins     as I

import           XmlFormatter

newtype TestInterpreter a = MockInterpreter {
    runInterpreter
        :: StateT (Environment Value) (
           Either RuntimeError) a
} deriving (Functor, Applicative, Monad, MonadError RuntimeError, MonadState (Environment Value))
-- | Always returns "test" on read and ignores output
instance MonadConsole TestInterpreter where
    consoleRead = return "test"
    consoleWrite = const (pure ())

-- Run a spec while handling some extra arguments
myHspec :: Spec -> IO ()
myHspec spec = do
    args <- getArgs
    let config = if "--junit-output" `elem` args
            then defaultConfig { configFormatter = Just xmlFormatter }
            else defaultConfig
    withArgs (filter (/= "--junit-output") args) $ hspecWith config spec

main :: IO ()
main = myHspec $ do
    -- Filenames and contents
    files <- runIO (readAll "test/src")

    context "Non-crash checks" $ do
        parallel $ describe "Parser" $ forM_ files $ \(name, content) ->
            it ("Parses " <> name) $ isJust (parseMaybe name content)

        describe "Desugarer" $ it "Does not crash" True

        parallel $ describe "Typechecker" $ forM_ files $ \(name, content) ->
            it ("Checks " <> name)
                $ isJust
                      (   parseMaybe name content
                      >>= (pure . D.desugar)
                      >>= typecheckMaybe
                      )

        parallel $ describe "Interpreter" $ forM_ files $ \(name, content) ->
            it ("Runs " <> name) $ isJust
                (   parseMaybe name content
                >>= (pure . D.desugar)
                >>= typecheckMaybe
                >>= runMaybe
                )


-- Utils

readAll :: FilePath -> IO [(String, String)]
readAll dir = listDirectory dir >>= traverse
    (\path -> do
        contents <- readFile (dir <> "/" <> path)
        return (extractName path, contents)
    )
  where
    extractName :: FilePath -> String
    extractName = reverse . takeWhile (/= '/') . reverse

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

parseMaybe :: String -> String -> Maybe ParsedProgram
parseMaybe = fmap eitherToMaybe . F.parseProgram

typecheckMaybe :: DesugaredProgram -> Maybe TypedProgram
typecheckMaybe = eitherToMaybe . T.typecheck

runMaybe :: TypedProgram -> Maybe ()
runMaybe =
    eitherToMaybe . (`evalStateT` I.baseEnv) . runInterpreter . I.runProgram
