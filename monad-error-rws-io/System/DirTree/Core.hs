module System.DirTree.Core
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.List                        ( isInfixOf, sort )

import System.DirTree.Types
import System.DirTree.FilePath
import System.DirTree.FileSystem
import System.DirTree.FindExpr

-- ----------------------------------------

initEnv :: Env
initEnv = Env
          { theProgName     = "processDirTree"
          , theRootDir      = "."
          , theCwd          = "."
          , theLevel        = 0
          , theUserFindExpr = FTrue
          , theSysFindExpr  = FTrue
          , theFindPred     = falsePred
          , theGrepPred     = const False
          , theSedFct       = id
          , theProcessor    = genFindProcessor
          , theTraceFlag    = False
          , theWarningFlag  = True
          , theStdErrFlag   = True
          , theUtf8DecFlag  = False
          , theUtf8EncFlag  = False
          , theCreateBackup = True
          , theBackupName   = (++ "~")
          }

initGrepPred :: Env -> Env
initGrepPred env
    = env { theGrepPred = ("True" `isInfixOf`)
          , theProcessor = genGrepProcessor
          -- , theUserFindExpr = matchExtRE "hs"
          }

initFindPred :: Env -> Env
initFindPred env
    = env { theFindPred = findExpr2FindPred $
                          andExpr (theSysFindExpr env) (theUserFindExpr env)
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

initState :: State
initState = ()

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
         predicate    <- asks theFindPred
         genProcessor <- asks theProcessor
         (start, action, finish)
                      <- genProcessor
         
         always $
           do start
              always  $
                pwd
                >>= getDirContents
                >>= return . sort . filter (`notElem` [".", ".."])
                >>= mapM_ (processEntry predicate action)
              finish
    where
      processEntry predicate action n
          = do always $
                 predicate n `guards`
                   do pathName n >>= trc . ("processing " ++) . show
                      action n

               always $
                 isDir n `guards`
                   withSubDir n traverseDirTree

withSubDir :: FilePath -> Cmd a -> Cmd a
withSubDir dir cmd
    = do cwd <- pwd
         res <- ( cd dir
                  >> local (extendCwdPath dir) cmd
                ) `finally` cd cwd
         return res

-- ----------------------------------------

genFindProcessor :: Cmd (Cmd (), FilePath -> Cmd (), Cmd ())
genFindProcessor
    = return (return (), pathName >=> io . putStrLn, return ())

genGrepProcessor :: Cmd (Cmd (), FilePath -> Cmd (), Cmd ())
genGrepProcessor
    = do grepPred <- asks theGrepPred
         return (return (), contentGrep grepPred, return ())

geSedProcessor :: Cmd (Cmd (), FilePath -> Cmd (), Cmd ())
geSedProcessor
    = do sedFct <- asks theSedFct
         return (return (), editFileContents sedFct, return ())

contentGrep     :: (String -> Bool) -> FilePath -> Cmd ()
contentGrep p f
    = do pn  <- pathName f
         xs <- test pn . lines <$> getFileContents f
         mapM_ (io . putStrLn) xs
    where
      test pn'
          = map format . filter (p . snd) . zip [(1::Int)..]
          where
            format (n, k) = pn' ++ ":" ++ show n ++ ": " ++ k

-- ----------------------------------------
