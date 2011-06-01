module DSL.Base
where

import System.IO

-- ------------------------------------------------------------

class IsQuitInstr a where
    isQuit :: a -> Maybe String
    isQuit _ = Nothing

class IsIllegalInstr a where
    isIllegal :: a -> Maybe String
    isIllegal _ = Nothing

class IsUsageInstr a where
    isUsage :: a -> Maybe String
    isUsage _ = Nothing

class IsIncompleteInstr a where
    isIncomplete :: a -> Maybe String
    isIncomplete _ = Nothing

class ( IsQuitInstr       a
      , IsIllegalInstr    a
      , IsUsageInstr      a
      , IsIncompleteInstr a
      ) =>
    IsInstr a where

-- ------------------------------------------------------------

class InitialValue a where
    initValue :: a

-- ------------------------------------------------------------

class ReadLine a where
    getReadLine :: a -> (String -> IO String)
    setReadLine :: (String -> IO String) -> a -> a

    getReadLine = const readLineSimple
    setReadLine = const id

readLineSimple :: String -> IO String
readLineSimple p
    = do l <- readLine'
         if null l
            then readLineSimple p
            else return l
    where
      readLine'
          = do hPutStr stdout p
               hFlush stdout
               getLine

-- ------------------------------------------------------------
