module Kima.Interface.ArgumentParser where

import Options.Applicative

data Command = Run RunOpts FilePath
             | Compile CompileOpts FilePath
    deriving Show

data RunOpts = RunOpts deriving Show
data CompileOpts = CompileOpts {
    output :: Maybe FilePath
} deriving Show

filepath :: Parser FilePath
filepath = argument str (metavar "FILENAME")

parseRun :: Parser Command
parseRun = Run <$> pure RunOpts <*> filepath

parseCompile :: Parser Command
parseCompile = Compile <$> compileOpts <*> filepath

compileOpts :: Parser CompileOpts
compileOpts = CompileOpts <$> optional (strOption (
    short 'o' 
    <> long "output" 
    <> metavar "OUTPUT" 
    <> help "Name of the output file"))

parseCommand :: Parser Command
parseCommand = hsubparser $ 
    command "run" (parseRun `info` progDesc "Run FILENAME") <>
    command "compile" (parseCompile `info` progDesc "Compile FILENAME")

getCommand :: IO Command
getCommand = customExecParser preferences parser 

getCommand' :: [String] -> ParserResult Command
getCommand' = execParserPure preferences parser

preferences = prefs showHelpOnEmpty 
parser = (parseCommand <**> helper) `info` 
    (fullDesc 
    <> header "The Kima programming language")