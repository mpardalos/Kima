module Kima.Interface.ArgumentParser where

import Options.Applicative

data Command = Run RunOpts FilePath
             | Compile CompileOpts FilePath
             | Repl
    deriving Show

data RunOpts = RunOpts deriving Show

data GlobalOpt

newtype CompileOpts = CompileOpts {
    output :: Maybe FilePath
} deriving Show

filepath :: Parser FilePath
filepath = argument str (metavar "FILENAME")

parseRun :: Parser Command
parseRun = Run RunOpts <$> filepath

parseCompile :: Parser Command
parseCompile = Compile <$> compileOpts <*> filepath

compileOpts :: Parser CompileOpts
compileOpts = CompileOpts <$> optional (strOption (
    short 'o'
    <> long "output"
    <> metavar "OUTPUT"
    <> help "Name of the output file"))

parseOptions :: Parser (a -> a)
parseOptions = helper <*> flag id id (mconcat [short 'h', long "help"])

parseCommand :: Parser Command
parseCommand =
    hsubparser
            (  command "run"     (parseRun `info` progDesc "Run FILENAME")
            <> command "compile" (parseCompile `info` progDesc "Compile FILENAME")
            <> command "repl"    (pure Repl `info` progDesc "Run repl")
            )
        <|> pure Repl

preferences = prefs showHelpOnEmpty

parser = (parseOptions <*> parseCommand) `info`
    (fullDesc
    <> header "The Kima programming language")

getCommand :: IO Command
getCommand = customExecParser preferences parser

getCommand' :: [String] -> ParserResult Command
getCommand' = execParserPure preferences parser
