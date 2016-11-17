module Catalog.Options
where

import Catalog.Cmd.Types
import Catalog.System.Convert (selectFont)
import Data.Maybe             (fromJust)
import Data.Prim.Prelude
import System.Console.CmdTheLine
import Text.PrettyPrint

mainWithArgs :: String -> (Env -> IO ()) -> IO ()
mainWithArgs theAppName theAppMain =
  run (theApp <$> oAll, appInfo theAppName)
  where
    theApp :: (Env -> Env) -> IO ()
    theApp setOpts = do
      -- compute default font for convert prog
      setFontName <- fontForConvert
      theAppMain (setOpts . setFontName $ env0)
        where
          env0 = -- the server defaults
            defaultEnv
            & envTrc     .~ False
            & envVerbose .~ True

fontForConvert :: IO (Env -> Env)
fontForConvert = do
  (res, _, _) <- runCmd selectFont
  let fn = either (const mempty) id res
  return (& envFontName .~ fn)

appInfo :: String -> TermInfo
appInfo app =
  defTI { termName = "catalog-" ++ app
        , version  = "0.1.0.0"
        }

oAll
  , oVerbose
  , oJournal
  , oTrc
  , oQuiet
  , oForceMDU
  , oPort
  , oMountPath
  , oArchive
  , oImport
  , oSyncDir :: Term (Env -> Env)

oAll =
  oVerbose
  <..> oJournal
  <..> oTrc
  <..> oQuiet
  <..> oForceMDU
  <..> oPort
  <..> oMountPath
  <..> oArchive
  <..> oImport
  <..> oSyncDir

oVerbose =
  convFlag setVerbose $
  (optInfo ["v", "verbose"])
    { optDoc = "Turn on verbose output." }
  where
    setVerbose True  = envVerbose .~ True
    setVerbose False = id

oJournal =
  convFlag setJournal $
  (optInfo ["j", "journal"])
    { optDoc = "Turn on journaling output." }
  where
    setJournal True  = envJournal .~ True
    setJournal False = id

oTrc =
  convFlag setTrc $
  (optInfo ["t", "trc"])
    { optDoc = "Turn on trace output." }
  where
    setTrc True  = envTrc .~ True
    setTrc False = id

oQuiet =
  convFlag setQuiet $
  (optInfo ["q", "quiet"])
    { optDoc = "Turn off verbose and trace output." }
  where
    setQuiet True  e = e & envVerbose .~ False
                         & envTrc     .~ False
    setQuiet False e = e

oForceMDU =
  convFlag setFMDU $
  (optInfo ["u", "force-metadata-update"])
    { optDoc = "Force metadata update when syncing catalog." }
  where
    setFMDU True  e = e & envForceMDU .~ True
    setFMDU False e = e

oPort =
  convStringValue "PORT must be a number" setPort $
  (optInfo ["p", "port"])
    { optName = "PORT"
    , optDoc  = "The port listened at by the catalog server. Default is 3001"
    }
  where
    setPort s
      | null s        = Just id
      | all isDigit s = Just (envPort .~ read s)
      | otherwise     = Nothing

oMountPath =
  convStringValue "not a legal mount path" setMP $
  (optInfo ["m", "mount-path"])
    { optName = "MOUNT-PATH"
    , optDoc = "The mount path for the whole archive, default \".\""
    }
  where
    setMP s
      | null s    = Just id
      | otherwise = Just (envMountPath .~ s)

oArchive =
  convStringValue "not a legal archive name" setArchive $
  (optInfo ["a", "archive"])
    { optName = "ARCHIVE"
    , optDoc  = "The JSON archive file to be loaded (relative to mount path)" ++
                ", default \"catalog.json\""
    }
  where
    setArchive s
      | null s    = Just id
      | otherwise = Just (envJsonArchive .~ s)

oImport =
  convStringValue "not a legal import name" setImport $
  (optInfo ["i", "import"])
    { optName = "IMPORT"
    , optDoc  = "For syncing only: The JSON import file for collections to be imported" ++
                ", default \"import.json\"." ++
                " If import file is given, just an import is done, no sync."
    }
  where
    setImport s
      | null s    = setImport "import.json"
      | otherwise = Just (envJsonImport .~ Just s)



oSyncDir =
  convStringValue "not a legal dir path" setMP $
  (optInfo ["d", "dir-path"])
    { optName = "DIR-PATH"
    , optDoc = "For syncing only: The dir path for the subdir to be synchronized, default \".\""
    }
  where
    setMP s
      | null s    = Just id
      | otherwise = Just (envSyncDir .~ s)


-- ----------------------------------------

(<..>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<..>) = liftA2 (.)

convFlag :: (Bool -> b) -> OptInfo -> Term b
convFlag setFct
    = fmap setFct . value . flag

convStringValue :: String -> (String -> Maybe (b -> b)) -> OptInfo -> Term (b -> b)
convStringValue msg setFct
    = convertSeqIO id (return . setFct) msg . fmap toList' . value . opt ""
    where
      toList' "" = []
      toList' s  = [s]

convDefaultStringValue :: String -> (String -> Maybe (b -> b)) -> String -> OptInfo -> Term (b -> b)
convDefaultStringValue msg setFct defaultValue
    = convertSeqIO id (return . setFct) msg . fmap toList' . value . defaultOpt defaultValue ""
    where
      toList' "" = []
      toList' s  = [s]

convStringSeqValue :: String -> (String -> Maybe (b -> b)) -> OptInfo -> Term (b -> b)
convStringSeqValue msg setFct
    = convertSeqIO id (return . setFct) msg . value . optAll []

-- ----------------------------------------

-- the general argument check function
--
-- Checked are all elements of a term with a list value.
-- These values are checked and if o.k. converted into a function (conv).
-- usually transforming an env into a new env.
-- The check funtion runs in the IO monad, so checks whether args
-- represent existing filesystem entries becomes possible.
--
-- For error messages a conversion function (showArg) into a string is neccessary.
-- If the values are already strings, id is a good candidate, else show is.

convertSeqIO :: (a -> String) -> (a -> IO (Maybe (b -> b))) -> String -> Term [a] -> Term (b -> b)
convertSeqIO showArg conv msg
    = ret . fmap check
    where
      check vs
          = do (rs, es) <- part <$> mapM (liftIO . conv) vs
               if null es
                  then return $ foldr (.) id rs
                  else msgFail $ (msg' $ head es)
          where
            part = (map (fromJust . snd) *** map fst) . partition (isJust . snd) . zip vs
            msg' v = sep [text msg, quotes . text . showArg $ v]

-- general function for checking and converting a single value
--
-- more general than 'convertSeqIO' but does not well for value lists
-- (options that may be repeated).

convertValueIO :: (a -> String) -> (a -> IO (Maybe b)) -> String -> Term a -> Term b
convertValueIO showArg conv msg
    = ret . fmap check
    where
      check v
          = do res <- liftIO . conv $ v
               case res of
                 Nothing -> msgFail $ msg'
                 Just r  -> return r
          where
            msg' = sep [text msg, quotes . text . showArg $ v]

-- -}

-- ----------------------------------------
