module CLI where

import RIO
import Options.Applicative

data Options = Options
  { runREPL :: Bool
  , file :: FilePath
  } deriving (Show)

optionsP :: Parser Options
optionsP = Options
      <$> switch
          ( long "repl"
         <> short 'r'
         <> help "start a REPL" )
      <*> (argument str (metavar "FILE"))

opts :: ParserInfo Options
opts = info (optionsP <**> helper) fullDesc
