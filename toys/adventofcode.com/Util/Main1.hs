module Util.Main1 (main1, main12) where

import Options.Applicative
import Data.Monoid         ((<>))

data Args = Args
  { _part2 :: Bool
  , _clinp :: Maybe String
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
  flag False True
  ( long "part2"
    <> short '2'
    <> help ( "exec part 2 of problem")
  )
  <*>
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

main1 :: String ->
         String -> (String -> String) -> IO ()
main1 pno x1 x2 = main12 pno x1 x2 x1 x2

main12 :: String ->
          String -> (String -> String) ->
          String -> (String -> String) ->
          IO ()
main12 pno defInput1 process1 defInput2 process2 = do
  args <- execParser opts
  let (defInput, process)
        | _part2 args = (defInput2, process2)
        | otherwise   = (defInput1, process1)

  inp  <- getInput defInput args
  putStrLn . process $ inp
  where
    (year, day') = span (/= '-') pno
    day          = dropWhile (`notElem` "123456789") day'

    opts = info (argsParser <**> helper)
      ( fullDesc
        <> ( progDesc $
             unlines
             [ "Solve problem \"Day " ++ day ++
               "\" of Advent of Code " ++ year ++
               " (http://www.adventofcode.com/" ++
               year ++ "/day/" ++ day ++ ")."
             ]
           )
        <> header
           ( pno ++ " - solve a problem of adventofcode.com")
      )

getInput :: String -> Args -> IO String
getInput defInp args
  | Just xs            <- _clinp args = return xs
  | Just (FromFile fn) <- _input args = readFile fn
  | Just FromStdin     <- _input args = getContents
  | otherwise                         = return defInp

-- ----------------------------------------
