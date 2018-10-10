module ArgumentParser where

import Options.Applicative

data Command = Run [RunOpts] FilePath
             | Compile [CompileOpts] FilePath

data RunOpts
data CompileOpts = OutputFilename String

parseRun :: Parser Command
parseRun = Run <$> pure [] <*> strOption (metavar "FILENAME")

parseCompile :: Parser Command
parseCompile = Compile <$> pure [] <*> strOption (metavar "FILENAME")

parseCommand :: Parser Command
parseCommand = subparser $ 
    command "run" (parseRun `info` progDesc "Run FILENAME") <>
    command "compile" (parseCompile `info` progDesc "Compile FILENAME")

getCommand :: IO Command
getCommand = execParser (parseCommand `info` progDesc "The Kima programming language")