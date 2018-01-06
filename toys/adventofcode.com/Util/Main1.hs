module Util.Main1 (main1) where

import Options.Applicative
import Data.Monoid         ((<>))

data Args = Args
  { _clinp :: Maybe String
  , _input :: Maybe InpArg
  }

data InpArg
  = FromFile String
  | FromStdin
  deriving (Show)

argsParser :: Parser Args
argsParser =
  Args
  <$>
  ( optional $ strOption
    ( long "input"
      <> short 'i'
      <> metavar "INPUT"
      <> help "input from the command line"
    )
  )
  <*>
  ( optional inpParser )

inpParser :: Parser InpArg
inpParser =
   ( fromFile
     <$>
     argument str
     ( metavar "FILE"
       <> help "input to be processed, \"-\" for stdin"
     )
   )
  where
    fromFile "-" = FromStdin
    fromFile fn  = FromFile fn

main1 :: String -> (String -> String) -> IO ()
main1 defInput process = do
  inp <- execParser opts >>= getInput defInput
  putStrLn . process $ inp
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
        <> ( progDesc $
             unlines
             [ "Solve a problem of adventofcode.com"
             ]
           )
        <> header "<year>-<day> - solve a problem of adventofcode.com"
      )

getInput :: String -> Args -> IO String
getInput defInp args
  | Just xs            <- _clinp args = return xs
  | Just (FromFile fn) <- _input args = readFile fn
  | Just FromStdin     <- _input args = getContents
  | otherwise                         = return defInp
