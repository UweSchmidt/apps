module System.DirTree.FilePath
where

import Control.Arrow    ( first )

-- ------------------------------

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

-- ------------------------------

splitFileName :: FilePath -> (FilePath, FilePath)
splitFileName pn
    = uncurry step0 . span (/= '/') . reverse $ pn
    where
      step0 _  ""     = ("", pn)
      step0 _  "/"    = ("/", tail pn)
      step0 "" (_:ys) = splitFileName $ reverse ys
      step0 xs (_:ys) = (reverse ys, reverse xs)

joinFileName :: FilePath -> FilePath -> FilePath
joinFileName _  fn@('/':_) = fn
joinFileName "" fn         = fn
joinFileName pn fn
    | pn == "/"            = pn ++ fn
    | last pn == '/'       = joinFileName (init pn) fn
    | otherwise            = pn ++ "/" ++ fn

getFileName :: FilePath -> FilePath
getFileName = snd . splitFileName

getDirPath :: FilePath -> FilePath
getDirPath = fst . splitFileName

-- ------------------------------

-- | split a filepath into base path and extension
--
-- If the extension is empty, the basename equals the filepath,
-- if the extension is not empty, the '.' between basename and extension is discarded
-- An extension never contains a '.'. A basename will never be null, except
-- the whole filename was nul

-- > splitNameExt "abc.123" == ("abc", "123")
-- > splitNameExt "abc"     == ("abc", "")
-- > splitNameExt "abc."    == ("abc.", "")
-- > splitNameExt "abc.1.2" == ("abc.1", "2")

splitExt :: FilePath -> (FilePath, FilePath)
splitExt pn
    = uncurry step0 . span (/= '/') . reverse $ pn
    where
      step0 xs ys = first (reverse ys ++) . splitNameExt $ xs

      splitNameExt fn
          = uncurry step1 . span (/= '.') $ fn
          where
            step1 _  ""  = (fn, "")             -- no extension there
            step1 _  "." = (fn, "")             -- dot file
            step1 "" _   = (fn, "")             -- empty extension
            step1 xs (_:ys) = (reverse ys, reverse xs)

joinExt :: FilePath ->  FilePath -> FilePath
joinExt fn ext
    | null fn   = ext
    | null ext  = fn
    | otherwise = fn ++ "." ++ ext

getExt :: FilePath -> FilePath
getExt = snd . splitExt

remExt :: FilePath -> FilePath
remExt = fst . splitExt

hasExt :: FilePath -> Bool
hasExt = not . null . snd . splitExt

-- uncurry joinExt . splitExt == id

remExts   :: [FilePath] -> FilePath -> FilePath
remExts es f
    | ext `elem` es
        = base
    | otherwise
        = f
    where
      (base, ext) = splitExt f

-- ------------------------------
