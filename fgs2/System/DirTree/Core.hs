module System.DirTree.Core
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.Char                        ( toLower )
import Data.List                        ( isInfixOf
                                        , sort
                                        )
import System.DirTree.Types
import System.DirTree.FilePath
import System.DirTree.FileSystem
import System.DirTree.FindExpr
import System.DirTree.Hash

-- ----------------------------------------

initEnv :: Env
initEnv = Env
          { theProgName      = "processDirTree"
          , theRootDir       = "."
          , theCwd           = "."
          , theLevel         = 0
          , theUserFindExpr  = FTrue
          , theSysFindExpr   = FTrue
          , theDirFindExpr   = FTrue
          , theFindPred      = falsePred
          , theDirPred       = falsePred
          , theGrepPred      = const False
          , theSedFct        = id
          , theProcessor     = genFindProcessor "{path}"
          , theFollowSymlink = False
          , theTraceFlag     = False
          , theWarningFlag   = True
          , theErrorFlag     = True
          , theStdErrFlag    = True
          , theUtf8DecFlag   = False
          , theUtf8EncFlag   = False
          , theCreateBackup  = True
          , theBackupName    = (++ "~")
          , theHashFct       = sha1Hash
          , theHashUpdate    = False
          , theChecksumFile  = ".sha1"
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
          , theDirPred  = findExpr2FindPred $
                          theDirFindExpr env
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
         dirPred      <- asks theDirPred
         genProcessor <- asks theProcessor
         (start, action, finish)
                      <- genProcessor

         always $
           do start
              always  $
                pwd
                >>= getDirContents
                >>= return . sort . filter (`notElem` [".", ".."])
                >>= mapM_ (processEntry predicate dirPred action)
              finish
    where
      processEntry predicate dirPred action n
          = do always $
                 predicate n `guards`
                   do pathName n >>= trc . ("processing " ++) . show
                      action n

               always $
                 isDir n `guards`
                   ( dirPred n `guards`
                       withSubDir n traverseDirTree )

withSubDir :: FilePath -> Cmd a -> Cmd a
withSubDir dir cmd
    = do cwd <- pwd
         res <- ( cd dir
                  >> local (extendCwdPath dir) cmd
                ) `finally` cd cwd
         return res

-- ----------------------------------------

genFindProcessor :: String -> Cmd (Cmd (), FilePath -> Cmd (), Cmd ())
genFindProcessor fmt
    = return ( return ()
             , pathName >=> format >=> io . putStrLn
             , return ()
             )
    where
      format path
          = return $ sedRE subst reParam fmt
          where
            subst "{base}"     = remExt . getFileName $ path
            subst "{ext}"      =          getExt        path
            subst "{dir}"      =          getDirPath    path
            subst "{name}"     =          getFileName   path
            subst "{path}"     =                        path

            subst "{lc-base}"  = remExt .
                                 toLC   . getFileName $ path
            subst "{lc-ext}"   = toLC   . getExt      $ path
            subst "{lc-dir}"   = toLC   . getDirPath  $ path
            subst "{lc-name}"  = toLC   . getFileName $ path
            subst "{lc-path}"  = toLC                   path

            subst x            = x

            toLC               = map toLower

genGrepProcessor :: Cmd (Cmd (), FilePath -> Cmd (), Cmd ())
genGrepProcessor
    = do grepPred <- asks theGrepPred
         return ( return ()
                , contentGrep grepPred
                , return ()
                )

genSedProcessor :: Cmd (Cmd (), FilePath -> Cmd (), Cmd ())
genSedProcessor
    = do sedFct <- asks theSedFct
         return ( return ()
                , editFileContents sedFct
                , return ()
                )

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
