module Photo2.ImageOperations
where

import           Control.Monad	( when )
import           Control.Monad.Error hiding ( liftIO )
import qualified Control.Monad.Error as ME
import qualified Data.Map as M

import           Photo2.ArchiveTypes
import           Photo2.FilePath

import           System	( system )
import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix	( getProcessID )
import           System.Time	( ClockTime
				, toCalendarTime
				, formatCalendarTime
				, getClockTime
				)
import           System.Locale	( defaultTimeLocale )

-- ------------------------------------------------------------

type IOE a	= ErrorT String IO a

-- ------------------------------------------------------------

-- lift IO commands to IOE commands and map IO errors to ErrorT errors

liftIO	:: IO a -> IOE a
liftIO a
    = do
      r <- ME.liftIO $
           catch ( do
		   r1 <- a
		   return (Right r1)
		 )
                 (\ err -> return (Left $ show err))
      evalRc r
    where
    evalRc (Left msg)	= throwError msg
    evalRc (Right res)	= return res

mapError	:: IOE a -> (String -> String) -> IOE a
mapError a f
    = a `catchError` (throwError . f)

-- ------------------------------------------------------------

theCopies	= ( picCopies, \ c x -> x { picCopies = c} )
theOrig		= ( picOrig,   \ o x -> x { picOrig   = o} )
theEdited	= ( picEdited, \ e x -> x { picEdited = e} )

-- ------------------------------------------------------------

importOrig	:: Config -> Path -> Pic -> IOE Pic
importOrig c p pic
    = do
      ex <- existsSrc
      when (not ex) (throwError $ "importOrig: original file " ++ show src ++ " does not exist")
      when (not dry) (mkDirectoryPath dst)
      up <- upToDate
      if not up
	 then do
	      copy
	      geo <- getGeo src
	      return $
		     change theEdited (const True) $
		     change theCopies (M.insert "org" (Copy geo)) pic
	 else return pic
    where
    existsSrc	= liftIO $  doesFileExist src
    copy	= do
		  execFct debug dry shellcmd
		  return ()
    getGeo _	= return (Geo 0 0)
    upToDate
	| force		= return False
	| otherwise	= liftIO $ fileNewerThanFile src dst

    src		= base </> picOrig pic
    dst		= "org" </> joinPath p `addExtension` "jpg"

    base	= getOpt "base"       c
    debug	= hasOpt optDebug     c
    dry		= hasOpt optDryRun    c
    force	= hasOpt optForceOrig c

    shellcmd
	| extension src == extension dst
	    = [ "cp", src, dst, "&&", "chmod", "644", dst ]
	| otherwise
	    = [ "convert", "-quality", "85", src, dst ]


-- ------------------------------------------------------------

formatDateTime	:: IO ClockTime -> IO String
formatDateTime timeStamp
    = catch
      ( do
	ctime <- timeStamp
	time  <- toCalendarTime ctime
	return $ formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
      )
      ( \_ -> return "" )

getTimeStamp	:: IO String
getTimeStamp	= formatDateTime getClockTime

-- | read the last modified time stamp of a file

fileLastModified	:: String -> IO String
fileLastModified f	= formatDateTime (getModificationTime f)

-- | compare 2 file stamps
--
-- @fileNewerThanFile reference file@
-- if reference does not exist, return False,
-- else if file does not exist, return False
-- else compare time stamps

fileNewerThanFile	:: String -> String -> IO Bool
fileNewerThanFile ref f
    = do
      mf   <- fileLastModified f
      mref <- fileLastModified ref
      let n = ( not (null mf)
	       &&
	       not (null mref)
	       &&
	       mref < mf
	     )
      -- putStrLn ("file newer file=" ++ show f ++ "," ++ show mf ++ " ref=" ++ show ref ++ "," ++ show mref ++ " status=" ++ show n)
      return n

fileNewerThanFiles	:: [String] -> String -> IO Bool
fileNewerThanFiles [] _f
    = return True
fileNewerThanFiles (r:refs) f
    = do
      newer <- fileNewerThanFile r f
      if newer
	 then fileNewerThanFiles refs f
	 else return False

-- | compare a file stamp with a time stamp
--
-- @fileNewerThanDate dateRef file@
-- if dateRef is empty, return False,
-- else if file does not exist, return True
-- else compare time stamps

fileNewerThanDate	:: String -> String -> IO Bool
fileNewerThanDate dref f
    = if null dref
      then return False
      else do
	   mf   <- fileLastModified f
	   return ( not (null dref)
		    &&
		    dref < mf
		  )

fileNewerThanDates	:: [String] -> String -> IO Bool
fileNewerThanDates [] _f
    = return True
fileNewerThanDates (r:refs) f
    = do
      newer <- fileNewerThanDate r f
      if newer
	 then fileNewerThanDates refs f
	 else return False

-- ------------------------------------------------------------
--
-- call of external programs
-- and file handling

-- | quote a string to be used as command line argument for a system call

addArg		:: String -> String
addArg ";"	= " ;"
addArg "|"	= " |"
addArg "||"	= " ||"
addArg "&&"	= " &&"
addArg ">"	= " >"
addArg "2>"	= " 2>"
addArg t	= " '" ++ concatMap (\ c -> if c == '\'' then "\\'" else [c]) t ++ "\'"

-- | execute a shell command and return exit code

exec		:: String -> IOE ()
exec cmd
    = do
      rc <- liftIO $ system cmd
      exit rc
    where
    exit ExitSuccess	   = return ()
    exit (ExitFailure rc ) = throwError ("command " ++ show cmd ++ " exited with rc=" ++ show rc)

-- ------------------------------------------------------------

execFct		:: Bool -> Bool -> [String] -> IOE String

execFct _ _ []
    = return ""

execFct debug dry (cmd : args)
    = do
      pid <- liftIO $ getProcessID
      let tmpName = "/tmp/album-" ++ show pid
      let errDev  = "/dev/null"
      let ioReDir = [">", tmpName, "2>", errDev]
      let command = cmd ++ concatMap addArg args
      if dry
	 then do liftIO $ hPutStrLn stderr ("dry run: " ++ command)
		 return ""
	 else do
	      when debug
                   ( liftIO $ hPutStrLn stderr ("executed: " ++ command) )
	      let command' = command ++ concatMap addArg ioReDir
	      exec command'
	      res <- catchError
		     ( do
		       rh  <- liftIO $ openFile tmpName ReadMode
		       res <- liftIO $ hGetContents rh
		       rmFile tmpName
		       return res
		     ) ( \ _ -> return "" )
	      return res

-- ------------------------------------------------------------

mkDirectoryPath		:: String -> IOE ()
mkDirectoryPath f
    = do
      ex <- liftIO $ doesDirectoryExist dir
      when (not ex)
           ( ( liftIO $ createDirectoryIfMissing True dir )
	     `mapError` (("createDirectory " ++ dir ++ " failed: ") ++)
	   )
    where
    dir = dirName f

-- ------------------------------------------------------------

rmFile		:: String -> IOE ()
rmFile f
    = ( do
	ex <- return True -- liftIO $ doesFileExist f
	when ex (liftIO $ removeFile f)
      )
      `mapError` (("remove " ++ show f ++ " failed: ") ++)

-- ------------------------------------------------------------
