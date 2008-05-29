module Photo2.FilePath
where

import           Photo2.ArchiveTypes (Path)

import qualified System.FilePath as FP

slash, dot, thisDir, superDir	:: String

slash		= "/"
dot		= "."
thisDir		= "."
superDir	= ".."

quote		:: String -> String
quote str	= "\"" ++ str ++ "\""

squote		:: String -> String
squote str	= "'" ++ str ++ "'"

(++:)		:: String -> String -> String
"" ++: b	= b
a ++: ""	= a
a ++: b		= a ++ ": " ++ b

(++.)		:: String -> String -> String
"" ++. b	= b
a ++. ""	= a
a ++. b		= a ++ ", " ++ b

isSlash		:: Char -> Bool
isSlash		= (== head slash)

isDot		:: Char -> Bool
isDot		= (== head dot)

extension	:: String -> String
extension	= reverse . (\ (f', s') -> if null s' then s' else f') . break isDot . takeWhile (not . isSlash) . reverse

dirName		:: String -> String
dirName		= reverse . (\ t -> if null t then thisDir else t) . drop 1 . dropWhile (not . isSlash) . reverse

topDirName	:: String -> String
topDirName	= (\ (d,r) -> if null r then thisDir else d) . span (not . isSlash)

baseName	:: String -> String
baseName	= reverse . takeWhile (not . isSlash) . reverse

removeExtension	:: String -> String
removeExtension	= substExtension ""

removeTopDirName	:: String -> String
removeTopDirName	= (\ (d,r) -> if null r then d else tail r) . span (not . isSlash)

substExtension	:: String -> String -> String
substExtension newExt
		= (++ newExt) . reverse . drop 1. dropWhile (not . isDot) . reverse

substTopDirName	:: String -> String -> String
substTopDirName newTop
		= (newTop </>) . removeTopDirName

substBaseName	:: String -> String -> String
substBaseName newName path
    = dirName path </> newName

joinPathsAndNorm	:: String -> String -> String
joinPathsAndNorm dn
    = normPath . (dn </>)

addBaseDir	:: String -> String -> String
addBaseDir base fn
    | null fn		= base
    | null base		= fn
    | isAbsP fn		= fn
    | otherwise		= base </> fn
    where
    isAbsP = (== slash) . take 1


pathFromTo	:: String -> String -> String
pathFromTo ref1 ref2
    | td1 == thisDir
	= ref2
    | td1 == td2
	= pathFromTo (removeTopDirName ref1) (removeTopDirName ref2)
    | otherwise
	= upDirs ref1 ref2
    where
    td1 = topDirName ref1
    td2 = topDirName ref2

upDirs		:: String -> String -> String
upDirs ref1 ref2
    | td1 == thisDir
	= ref2
    | otherwise
	= upDirs (removeTopDirName ref1) (superDir ++ slash ++ ref2)
    where
    td1 = topDirName ref1

splitPath	:: String -> [String]
splitPath s
    | null s = []
    | null r = [b]
    | otherwise = b : splitPath (tail r)
    where
    (b, r) = break (== head slash) s

normPath	:: String -> String
normPath
    = listToPath . normPs [] . splitPath
    where
    normPs rl []
	= reverse rl
    normPs rl (r2:rs)
	| r2 == thisDir			-- simplify ./xxx
	    = normPs rl rs
    normPs (r1:rl) (r2:rs)
	| r2 == superDir
	  &&
	  r1 /= superDir
          &&
	  not (null r1)
	      = normPs rl rs		-- simplify  xxx/..
    normPs rl (r2:rs)
	= normPs (r2:rl) rs

pathToList	:: String -> [String]
pathToList	= dropWhile (== thisDir) . splitPath . normPath 

listToPath	:: [String] -> String
listToPath []	= thisDir
listToPath l	= foldr1 (\ x y -> x ++ slash ++ y) $ l

-- listToPath . pathToList == id

isAbsPath		:: [String] -> Bool
isAbsPath ("" : _)	= True
isAbsPath _		= False

showN	:: Int -> Int -> String
showN n
    = reverse . take n . reverse . ((replicate n '0') ++ ) . show

-- ------------------------------------------------------------

(</>)		:: String -> String -> String
(</>)		= FP.combine

joinPath	:: [FilePath] -> FilePath
joinPath	= FP.joinPath

addExtension	:: String -> String -> String
addExtension f e
    | null e	= f
    | otherwise	= FP.addExtension f e

-- ------------------------------------------------------------
