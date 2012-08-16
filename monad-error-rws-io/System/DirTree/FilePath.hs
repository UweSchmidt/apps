module System.DirTree.FilePath
where

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
