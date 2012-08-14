module Main
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.List                        ( isSuffixOf
                                        , sort
                                        )

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Files

import Text.Regex.XMLSchema.String	( match )

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C

-- ----------------------------------------

main :: IO ()
main
    = do args <- getArgs
         prog <- getProgName
         env <- evalOptions prog args initEnv
         res <- evalAction doIt env initState
         maybe exitFailure return res

-- ----------------------------------------

data Env
    = Env
      { theProgName     :: String
      , theRootDir      :: String
      , theCwd          :: String
      , theLevel        :: Int
      , theUserFindExpr :: FindExpr
      , theSysFindExpr  :: FindExpr
      , theFindPred     :: FindPred
      , theAction       :: String -> Cmd ()
      }

instance Config Env where

initEnv :: Env
initEnv = Env
          { theProgName     = "checksum"
          , theRootDir      = "."
          , theCwd          = "."
          , theLevel        = 0
          , theUserFindExpr = FTrue
          , theSysFindExpr  = NotExpr $ MatchRE "[.]{1,2}"	-- exclude . and ..
          , theFindPred     = falseFct
          , theAction       = pathName >=> io . putStrLn 
          }

initFindPred :: Env -> Env
initFindPred env
    = env { theFindPred = findExpr2FindPred $
                          AndExpr [ theSysFindExpr env
                                  , theUserFindExpr env
                                  ]
          }

initCwd :: Env -> Env
initCwd env
    = env { theCwd = theRootDir env }

extendCwdPath :: FilePath -> Env -> Env
extendCwdPath dir env
    = env { theCwd   = theCwd env `joinFile` dir
          , theLevel = theLevel env + 1
          }

-- ----------------------------------------

type State = ()

initState :: State
initState = ()

-- ----------------------------------------

type Cmd = Action Env State

-- ----------------------------------------

evalOptions :: String -> [String] -> Env -> IO Env
evalOptions pn args env0
    = return $ initFindPred . initCwd
             $ setOptions args
             $ env0 { theProgName = pn }
      where
        setOptions = const id

-- ----------------------------------------

doIt :: Cmd ()
doIt
    = do trc "script started"
         asks theRootDir >>= cd
         pwd >>= (trc . ("working dir is " ++))
         traverseDirTree
         trc "script finished"

traverseDirTree :: Cmd ()
traverseDirTree
    = do pwd >>= trc . ("scan directory " ++) . show
         predicate <- asks theFindPred
         action    <- asks theAction
         pwd
           >>= io . getDirectoryContents
           >>= return . sort . filter (`notElem` [".", ".."])
           >>= mapM_ (processEntry predicate action)
    where
      processEntry predicate action n
          = do matches <- predicate n
               when matches $ do pathName n >>= trc . ("processing " ++) . show
                                 action n
               isSubDir <- isDir n
               when isSubDir $ withSubDir n traverseDirTree

-- ----------------------------------------

cd :: FilePath -> Cmd ()
cd p
    = do io . setCurrentDirectory $ p
         pwd >>= trc . ("cwd " ++) . show

pwd :: Cmd String
pwd = io getCurrentDirectory

withSubDir :: FilePath -> Cmd a -> Cmd a
withSubDir dir cmd
    = do cwd <- pwd
         cd dir
         res <- local (extendCwdPath dir) cmd
         cd cwd
         return res

pathName :: FilePath -> Cmd FilePath
pathName name
    = (`joinFile` name) <$> asks theCwd

isDir :: FilePath -> Cmd Bool
isDir f
    = do
      s <- io $ getSymbolicLinkStatus f 	-- followSymLinks: getFileStatus
      return (isDirectory s)


-- ----------------------------------------

type FindPred = String -> Cmd Bool

data FindExpr
    = FPred       FindPred
    | Ext         String
    | Name        String
    | PathName     String
    | MatchRE     String
    | MatchPathRE String
    | FTrue
    | FFalse
    | IsFile
    | IsDir
    | HasCont     FindPred
    | AndExpr    [FindExpr]
    | OrExpr     [FindExpr]
    | NotExpr     FindExpr

-- ------------------------------

findExpr2FindPred :: FindExpr -> FindPred

findExpr2FindPred (FPred p) f
    = p f

findExpr2FindPred (Ext ext) f
    = return $ ext `isSuffixOf` f

findExpr2FindPred (Name f1) f
    = return (f1 == basename f)

findExpr2FindPred (PathName f1) f
    = pathName f >>= findExpr2FindPred (MatchRE f1)

findExpr2FindPred (MatchRE re) f
    = return . match re $ f

findExpr2FindPred (MatchPathRE re) f
    = pathName f >>= return . match re

findExpr2FindPred FTrue f
    = trueFct f

findExpr2FindPred FFalse f
    = falseFct f

findExpr2FindPred IsFile f
    = do
      s <- io $ getSymbolicLinkStatus f 	-- followSymLinks: getFileStatus
      return (isRegularFile s)

findExpr2FindPred IsDir f
    = do
      s <- io $ getSymbolicLinkStatus f 	-- followSymLinks: getFileStatus
      return (isDirectory s)

findExpr2FindPred (HasCont p) f
    = (findExpr2FindPred IsFile) `andFct` contentFind p $ f

findExpr2FindPred (AndExpr fl) f
    = (foldl andFct trueFct . map findExpr2FindPred) fl f

findExpr2FindPred (OrExpr fl) f
    = (foldl orFct falseFct . map findExpr2FindPred) fl f

findExpr2FindPred (NotExpr e) f
    = do
      r <- (findExpr2FindPred e) f
      return (not r)
-- ------------------------------

contentFind     :: FindPred -> FindPred
contentFind p f
    = do trc $ "contentFind: reading file " ++ show f
         cont <- io $ do h <- openFile f ReadMode
                         b <- B.hGetContents h
                         hClose h
                         return $ C.unpack b
         p cont

-- ------------------------------

trueFct :: FindPred
trueFct = return . const True

falseFct :: FindPred
falseFct  = return . const False

andFct  :: FindPred -> FindPred -> FindPred
andFct fct1 fct2 f
    = do
      r1 <- fct1 f
      if r1
         then fct2 f
         else return False

orFct   :: FindPred -> FindPred -> FindPred
orFct fct1 fct2 f
    = do
      r1 <- fct1 f
      if r1
         then return True
         else fct2 f

-- ------------------------------

-- filename manipulation

joinFile        :: FilePath -> FilePath -> FilePath
joinFile "" f   = f
joinFile d ""   = d
joinFile d f    = d ++ "/" ++ f

basename        :: FilePath -> FilePath
basename        = reverse . takeWhile (/= '/') . reverse

dirname         :: FilePath -> FilePath
dirname         = reverse . drop 1 . dropWhile (/= '/') . reverse

extension       :: FilePath -> FilePath
extension
    = reverse . takeWhile (/= '.') . reverse
      . hasDot . basename
    where
    hasDot s
        | all (== '.') s = ""
        | all (/= '.') s = ""
        | head s == '.'  = hasDot (tail s)
        | otherwise      = s

remTopDir       :: FilePath -> FilePath
remTopDir       = drop 1 . dropWhile (/= '/')

remExtensions   :: [String] -> FilePath -> FilePath
remExtensions es f
    | extFound
        = reverse . drop (length fe + 1) . reverse $ f
    | otherwise
        = f
    where
    fe = extension f
    extFound = fe `elem` es

-- ------------------------------
