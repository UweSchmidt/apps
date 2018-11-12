module Catalog.Options
where

import Catalog.Cmd.Types
import Catalog.System.Convert (selectFont)
import Data.Prim
import Options.Applicative

-- ----------------------------------------

mainWithArgs :: String -> (Env -> IO ()) -> IO ()
mainWithArgs theAppName theAppMain = do
  execParser (appInfo theAppName)
  >>= fontForConvert
  >>= theAppMain

-- convert needs a font for generating
-- collection icons from tiltle text or collection name

fontForConvert :: Env -> IO Env
fontForConvert env = do
  (res, _) <- runCmd selectFont
  let fn = either (const mempty) id res
  return (env & envFontName .~ fn)

appInfo :: String -> ParserInfo Env
appInfo pname =
  info (envp <**> helper)
  ( fullDesc
    <> progDesc "organize your photos"
    <> header ("catalog-" ++ pname ++ " - 0.1.2.0")
  )

envp :: Parser Env
envp = mkEnv
  <$> ( flag (defaultEnv ^. envTrc) True
        ( long "trc"
          <> short 't'
          <> help "Turn on trace output"
        )
      )
  <*> ( flag (defaultEnv ^. envVerbose) True
        ( long "verbose"
          <> short 'v'
          <> help "Turn on verbose output"
        )
      )
  <*> switch
      ( long "journal"
        <> short 'j'
        <> help "Turn on journaling archive changes"
      )
  <*> switch
      ( long "dry-run-jpg"
        <> short 'y'
        <> help "Dry run for generating .jpg images"
      )
  <*> switch
      ( long "force-meta-data-update"
        <> short 'f'
        <> help "Force metadata update when syncing catalog"
      )
  <*> option auto
      ( long "port"
        <> short 'p'
        <> help "The port listened at by the catalog server"
        <> showDefault
        <> value 3001
        <> metavar "PORT" )
  <*> strOption
      ( long "archive"
        <> short 'a'
        <> metavar "ARCHIVE"
        <> showDefault
        <> value "catalog.json"
        <> help "The JSON archive file to be loaded, relative to mount path"
      )
  <*> ( optional $
        strOption
        ( long "import"
          <> short 'i'
          <> metavar "IMPORT"
          <> help ( "For syncing only: The JSON import file for collections to be imported"
                    ++ " from old Album2 image organizer."
                    ++ " If import file is given, just an import is done, no sync"
                  )
        )
      )
  <*> ( mkSysPath <$>
        strOption
        ( long "mount-path"
          <> short 'm'
          <> metavar "MOUNT-PATH"
          <> showDefault
          <> value "."
          <> help "The mount path for the whole archive"
        )
      )
  <*> strOption
      ( long "dir-path"
        <> short 'd'
        <> metavar "DIR-PATH"
        <> showDefault
        <> value (defaultEnv ^. envSyncDir)
        <> help "For syncing only: The dir path for the subdir to be synchronized"
      )
  <*> pure (defaultEnv ^. envFontName)
  <*> pure (defaultEnv ^. envLogOp)
  <*> ( optional $
        strOption
        ( long "update-cache"
          <> short 'c'
          <> metavar "IMGDIR"
          <> help "For catalog-sync only: Update the image cache for given image directory"
        )
      )
  <*> switch
      ( long "save-hash-and-path-ix"
        <> help "Save both hash and path index version of catalog"
      )

-- ----------------------------------------
