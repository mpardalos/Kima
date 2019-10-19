module ArgumentParser (Command(..), RunOpts, CompileOpts, getCommand, getCommand') where

import           Options.Applicative

-- | Top-level commands in the CLI
data Command
    = Run RunOpts FilePath
    | Compile CompileOpts FilePath
    | Repl
    deriving Show

-- | Options relating to running (interpreting) code
data RunOpts = RunOpts
    deriving Show

-- | Options relating to compiling code
newtype CompileOpts = CompileOpts {
    -- | Where to write the compiled output. Can be omitted
    output :: Maybe FilePath
} deriving Show

-- | Top-level command parser
parseCommand :: Parser Command
parseCommand =
    hsubparser
            (  command "run"     (parseRun `info` progDesc "Run FILENAME")
            <> command "compile" (parseCompile `info` progDesc "Compile FILENAME")
            <> command "repl"    (parseRepl `info` progDesc "Run repl")
            )
        <|> pure Repl
  where
    parseRun :: Parser Command
    parseRun = Run RunOpts <$> filepath

    parseCompile :: Parser Command
    parseCompile = Compile <$> compileOpts <*> filepath

    compileOpts :: Parser CompileOpts
    compileOpts = CompileOpts <$> optional
        (strOption
            (mconcat [short 'o', long "output", metavar "OUTPUT", help "Name of the output file"])
        )

    parseRepl :: Parser Command
    parseRepl = pure Repl

filepath :: Parser FilePath
filepath = argument str (metavar "FILENAME")

withHelpFlag :: Parser (a -> a)
withHelpFlag = helper <*> flag id id (mconcat [short 'h', long "help"])

parser :: ParserInfo Command
parser = (withHelpFlag <*> parseCommand)
    `info` mconcat [fullDesc, header "The Kima programming language"]

getCommand :: IO Command
getCommand = execParser parser

getCommand' :: [String] -> ParserResult Command
getCommand' = execParserPure (prefs mempty) parser
