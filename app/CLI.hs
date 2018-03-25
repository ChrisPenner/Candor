module CLI where

import RIO
import Options.Applicative

data Options = Options
  { runREPL :: Bool
  }

optionsP :: Parser Options
optionsP = Options
      <$> switch
          ( long "repl"
         <> short 'r'
         <> help "start a REPL" )

opts :: ParserInfo Options
opts = info (optionsP <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
