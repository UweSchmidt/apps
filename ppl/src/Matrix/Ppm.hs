-- -------------------------------------------------------------------
--
-- portable bit/grey/pixel map parsing
-- and conversion to/from a Picture
--
-- $Id: Ppm.hs,v 1.7 2001/12/27 15:59:32 uwe Exp $

module Matrix.Ppm( PNM
          , readPicture
          , readPNM
          , pictureToPGM
          , pictureToPBM
          , showPNM
          , showPNMbin
          , showPicture
          ) where

import Data.Char(ord,chr)

import Matrix.Picture

data PNM        = Pbm Int Int     (Matrix Int)
                | Pgm Int Int Int (Matrix Int)
                | Ppm Int Int Int (Matrix (Int, Int, Int))

--

readPicture :: String -> Picture

readPicture = readPNM . item

--

readPNM :: (String, String) -> Picture

-- portable bitmap ascii

readPNM ("P1", str)
    = mapMx normalize pixels
      where
      (ns, rest)        = readRow 2 str
      [width, height]   = (map read ns)::[Int]
      (pixels, _)       = readPixels width height rest
      normalize x       = 1.0 - (read x)::Double

-- portable greymap ascii

readPNM ("P2", str)
    = mapMx normalize pixels
      where
      ([w, h, m], rest) = readRow 3 str
      width             = (read w)::Int
      height            = (read h)::Int
      maxi              = fromInteger ((read m)::Integer) ::Double
      normalize x       = ((read x)::Double) / maxi
      (pixels, _)       = readPixels width height rest

-- portable pixmap ascii

readPNM ("P3", str)
    = mapMx normalize pixels
      where
      ([w, h, m], rest) = readRow 3 str
      width             = (read w)::Int
      height            = (read h)::Int
      max3              = (fromInteger ((read m)::Integer) ::Double) * 3.0
      normalize (x1, x2, x3)
          = ( (fromInteger . toInteger) (((read x1)::Int)
                      + ((read x2)::Int)
                      + ((read x3)::Int)
                      )
            ) / max3
      (pixels, _)       = readPixels3 width height rest

-- portable grey map raw

readPNM ("P5", str)
    = mapMx normalize pixels
      where
      ([w, h, m], rest) = readRow 3 str
      width             = (read w)::Int
      height            = (read h)::Int
      maxi              = fromInteger ((read m)::Integer) ::Double
      normalize x       = (fromInteger . toInteger . ord) x / maxi
      pixels            = readChars width height (tail rest)

-- portable pixmap raw

readPNM ("P6", str)
    = mapMx normalize pixels
      where
      ([w, h, m], rest) = readRow 3 str
      width             = (read w)::Int
      height            = (read h)::Int
      max3              = (fromInteger ((read m)::Integer) ::Double) * 3.0
      normalize (x1, x2, x3)
          = (fromInteger . toInteger) (ord x1 + ord x2 + ord x3) / max3
      pixels            = read3Chars width height (tail rest)

readPNM (_,_)
    = [[0.0]]

--

item    :: String -> (String, String)
item str
    = i1
      where
      i@(w,r) = head (lex str)
      i1 | head w == '#'
             = item r1
         | otherwise
             = i
         where r1 = ( drop 1 . snd . break (== '\n') . drop 1) r

item3   :: String -> ((String, String, String), String)
item3 str
    = ((i1, i2, i3), str3)
      where
      (i1, str1) = item str
      (i2, str2) = item str1
      (i3, str3) = item str2

readFct :: (String -> (a, String)) -> Int -> String -> ([a], String)
readFct _ 0 str
    = ([],str)

readFct f n str
    = (i:r, str2)
      where
      (i, str1) = f str
      (r, str2) = readFct f (n-1) str1

readRow         :: Int -> String -> ([[Char]],String)
readRow
    = readFct item

readPixels      :: Int -> Int -> String -> ([[String]], String)
readPixels width
    = readFct (readRow width)

readRow3        :: Int -> String -> ([(String, String, String)], String)
readRow3
    = readFct item3

readPixels3     :: Int -> Int -> String -> ([[(String, String, String)]], String)
readPixels3 width
    = readFct (readRow3 width)

-- convert a binary pgm to a matrix

readChars :: Int -> Int -> String -> Matrix Char
readChars _ 0 _
    = []

readChars w h s
    = (take w s) : (readChars w (h-1) (drop w s))

read3Chars :: Int -> Int -> String -> Matrix (Char, Char, Char)
read3Chars _ 0 _
    = []

read3Chars w h s
    = (take3 w s) : (read3Chars w (h-1) (drop3 w s))
      where
      take3 0 _
          = []
      take3 n (c1:c2:c3:s1)
          = (c1, c2, c3) : take3 (n - 1) s1
      take3 _ _
          = []
      drop3 w1 = drop (3 * w1)

-- -------------------------------------------------------------------
--
-- Picture to portable grey map and portable bitmap (ascii)
-- conversions

pictureToPGM    :: Picture -> PNM
pictureToPGM pic
    = Pgm w h 255 (mapMx (\x -> round (x * 255.0)) pic)
      where
      w = length (head pic)
      h = length pic

pictureToPBM    :: Picture -> PNM
pictureToPBM    = toPBM . pictureToPGM

-- -------------------------------------------------------------------
--
-- portable ??? map conversion functions

-- convert to PGM format
-- 1. PGM: identity
-- 2. PBM to PGM
-- nothing else (not yet)

toPGM   :: PNM -> PNM
toPGM (Pbm w h pxs)
    = Pgm w h 1 (mapMx (\x -> 1 - x) pxs)

toPGM p@(Pgm _ _ _ _)
    = p

toPGM (Ppm w h c pxs)
    = Pgm w h (3 * c) (mapMx (\(x1,x2,x3) -> x1 + x2 + x3) pxs)

-- -------------------------------------------------------------------
--
-- convert to PBM format
-- 1. PBM: identity
-- 2. PGM to PBM
-- else first convert to PGM

toPBM   :: PNM -> PNM
toPBM p@(Pbm _ _ _)
    = p

toPBM (Pgm w h maxi pxs)
    = Pbm w h (mapMx mapTo01 pxs)
      where mapTo01 x   | x > maxi `div` 2      = 0
                        | otherwise             = 1

toPBM p@(Ppm _ _ _ _)
    = toPBM (toPGM p)

--------------------------------------------------------------------------------
--
-- output functions for .pbm and .pgm

showPNM,
 showPNMbin     :: PNM -> String

-- portable bit map format (ascii)

showPNM (Pbm width height pixels)
    =
      "P1\n"
      ++ show width ++ " " ++ show height ++ "\n"
      ++ version
      ++ concatMap (concatMap ((++ "\n") . show)) pixels

-- portable grey map format (ascii)

showPNM (Pgm width height maxi pixels)
    =
      "P2\n"
      ++ show width ++ " " ++ show height ++ "\n"
      ++ version
      ++ show maxi ++ "\n"
      ++ concatMap (concatMap ((++ "\n") . show)) pixels

showPNM (Ppm _ _ _ _)
    = error "no ppm ascii format supported"

-- portable grey map format (binary)

showPNMbin (Pgm width height maxi pixels)
    =
      "P5\n"
      ++ show width ++ " " ++ show height ++ "\n"
      ++ version
      ++ show maxi ++ "\n"
      ++ concatMap (map chr) pixels

showPNMbin (Pbm _ _ _)
    = error "no pbm binary output (magic number P4) supported"

showPNMbin (Ppm _ _ _ _)
    = error "no ppm binary output (magic number P6) supported"

-- private "portable float map" (ascii) for debugging

showPicture     :: Picture -> String
showPicture p
    =
      "Pf\n"
      ++ show (length (head p)) ++ " " ++ show (length p) ++ "\n"
      ++ version
      ++ "# internal floating point format\n"
      ++ concatMap (concatMap ((++ "\n") . show)) p

version :: String
version = "# Haskell PPM Tools $Id: Ppm.hs,v 1.7 2001/12/27 15:59:32 uwe Exp $\n"

-- -------------------------------------------------------------------
