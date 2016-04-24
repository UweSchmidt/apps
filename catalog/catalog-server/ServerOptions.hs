module ServerOptions
where

import Data.Prim.Prelude
import Catalog.Cmd.Types
import System.Console.CmdTheLine
import Data.Maybe
-- import Data.Char (isDigit)
import Text.PrettyPrint

mainWithArgs :: (Env -> IO ()) -> IO ()
mainWithArgs theServer = run (theApp <$> oAll, appInfo)
  where
    theApp :: (Env -> Env) -> IO ()
    theApp setOpts = do
      theServer (setOpts defaultEnv)

appInfo :: TermInfo
appInfo =
  defTI { termName = "catalog-server"
        , version  = "0.1.0.0"
        }

oAll
  , oVerbose
  , oTrc
  , oPort
  , oMountPath
  , oArchive :: Term (Env -> Env)

oAll =
  oVerbose
  <..> oTrc
  <..> oPort
  <..> oMountPath
  <..> oArchive

oVerbose =
  convFlag (envVerbose .~) $
  (optInfo ["v", "verbose"]) { optDoc = "Turn on verbose output." }

oTrc =
  convFlag (envTrc .~) $
  (optInfo ["t", "trc"]) { optDoc = "Turn on trace output." }

oPort =
  convStringValue "PORT must be a number" setPort $
  (optInfo ["p", "port"]) { optName = "PORT"
                          , optDoc  = "The port listened at. Default is 3001"
                          }
  where
    setPort s
      | null s        = Just id
      | all isDigit s = Just (envPort .~ read s)
      | otherwise     = Nothing

oMountPath =
  convStringValue "not a legal mount path" setMP $
  (optInfo ["m", "mount-path"]) { optName = "MOUNT-PATH"
                           , optDoc  = "The mount path for the whole archive, default \"./data\""
                           }
  where
    setMP s
      | null s    = Just id
      | otherwise = Just (envMountPath .~ s)

oArchive =
  convStringValue "not a legal archive name" setArchive $
  (optInfo ["a", "archive"]) { optName = "ARCHIVE"
                        , optDoc  = "The JSON archive file to be loaded (relative to mount path), default \"catalog.json\""
                        }
  where
    setArchive s
      | null s    = Just id
      | otherwise = Just (envJsonArchive .~ s)

-- ----------------------------------------

(<..>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<..>) = liftA2 (.)

convFlag :: (Bool -> b) -> OptInfo -> Term b
convFlag setFct
    = fmap setFct . value . flag

convStringValue :: String -> (String -> Maybe (b -> b)) -> OptInfo -> Term (b -> b)
convStringValue msg setFct
    = convertSeqIO id (return . setFct) msg . fmap toList . value . opt ""
    where
      toList "" = []
      toList s  = [s]

convDefaultStringValue :: String -> (String -> Maybe (b -> b)) -> String -> OptInfo -> Term (b -> b)
convDefaultStringValue msg setFct defaultValue
    = convertSeqIO id (return . setFct) msg . fmap toList . value . defaultOpt defaultValue ""
    where
      toList "" = []
      toList s  = [s]

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
