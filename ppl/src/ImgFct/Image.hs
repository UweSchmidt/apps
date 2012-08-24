module ImgFct.Image
where

import Data.Array.Unboxed

import Data.List
    ( mapAccumL
    )
import Data.Word

import System.FilePath
    ( takeExtension
    , replaceExtension
    )
import System.Directory
    ( doesFileExist
    )

import           System.IO
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

-- ----------------------------------------

type Lightness  = Double

type ColorDepth = Int

type Channel ix         = ix -> ix -> Lightness
type RasterChannel      = Channel Int
type ContChannel        = Channel Double

data Colored a
    = Grey  a
    | RGB   a a a
    | Alpha a (Colored a)

type ColorChannel ix    = Colored (Channel ix)

data Geo ix
    = Geo { width  :: !ix
          , height :: !ix
          }
      deriving (Eq)

type Raster     = Geo Int
type Cont       = Geo Double

data Image ix a
    = Image { imgGeo :: !(Geo ix)
            , imgCol ::  (Colored a)
            }

type ImgMap ix  = Image ix (Channel ix)
type PixMap     = ImgMap Int
type ContMap    = ImgMap Double

-- ----------------------------------------

-- mothers little helpers

on2             :: (a -> b) ->
                   (c -> d -> a) -> (c -> d -> b)
f `on2` c       = \ x y -> f (c x y)

merge           :: (a -> b -> c) ->
                   (x -> y -> a) ->
                   (x -> y -> b) ->
                   (x -> y -> c)

merge op f1 f2  = \ x y -> f1 x y `op` f2 x y

-- ----------------------------------------

-- predefined color depths

oneBit, eightBits, sixteenBits, defaultColorDepth :: ColorDepth

oneBit            =  1
eightBits         =  8
sixteenBits       = 16
defaultColorDepth = eightBits

-- ----------------------------------------

-- predefined Lightness values

dark            :: Lightness
dark            = 0.0

light           :: Lightness
light           = 1.0

grey            :: Lightness
grey            = 0.5

lightOrDark     :: Lightness -> Lightness
lightOrDark c
    | c <= 0.5  = 0.0
    | otherwise = 1.0

gammaLight      :: Lightness -> Lightness -> Lightness
gammaLight g x  = x ** (1/g)

invertLight     :: Lightness -> Lightness
invertLight     = (1.0 -)

reduceLight     :: Int -> Lightness -> Lightness
reduceLight n c = fromIntegral c' / fromIntegral (n - 1)
                where
                c' :: Int
                c' = floor (fromIntegral n * c)

{-
addLights       :: Lightness -> Lightness -> Lightness -> Lightness
addLights a c d = a * c + (1 - a) * d
-}

-- ----------------------------------------

-- basic Colored functions

-- map channels, alpha is mapped separately

mapColored2     :: (a -> b) -> (a -> b) -> (Colored a -> Colored b)
mapColored2 _  f (Grey x)       = Grey (f x)
mapColored2 _  f (RGB r g b)    = RGB (f r) (f g) (f b)
mapColored2 fa f (Alpha a i)    = Alpha (fa a) (mapColored2 fa f i)

-- map all channels equally

mapColored      :: (a -> b) -> (Colored a -> Colored b)
mapColored f    = mapColored2 f f

instance Functor Colored where
    fmap        = mapColored

-- map all except the alpha channel

mapColored'     :: (a -> a) -> (Colored a -> Colored a)
mapColored'     = mapColored2 id

-- zip two equally structured Colored values, alpha and other channels are zipped with different ops

zipColored2     :: (a -> a -> c) -> (a -> a -> c) -> (Colored a -> Colored a -> Colored c)
zipColored2 _  op (Grey x1)      (Grey x2)      = Grey $ x1 `op` x2
zipColored2 _  op (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 `op` r2) (g1 `op` g2) (b1 `op` b2)
zipColored2 oa op (Alpha a1 i1)  (Alpha a2 i2)  = Alpha (a1 `oa` a2) (zipColored2 oa op i1 i2)
zipColored2 oa op i1             i2             = uncurry (zipColored2 oa op) (unifyColored i1 i2)

zipColored      :: (a -> a -> c) -> (Colored a -> Colored a -> Colored c)
zipColored op   = zipColored2 op op

-- fold a colored value
-- 1. arg folds Grey, 2. arg RGB, 3. arg Alpha component

foldColored     :: (a -> b) ->
                   (a -> a -> a -> b) ->
                   (a -> b -> b) ->
                   (Colored a -> b)
foldColored gf _    _  (Grey x) = gf x
foldColored _  rgbf _  (RGB r g b) = rgbf r g b
foldColored gf rgbf af (Alpha a i) = af a (foldColored gf rgbf af i)


unifyColored    :: Colored a -> Colored a -> (Colored a, Colored a)
unifyColored (Grey x)    c2@(RGB _ _ _) = (RGB x x x, c2       )
unifyColored c1@(RGB _ _ _) (Grey x)    = (c1       , RGB x x x)
unifyColored (Alpha a1 i1) (Alpha a2 i2)= let
                                          (i1', i2') = unifyColored i1 i2
                                          in
                                          (Alpha a1 i1', Alpha a2 i2')
unifyColored (Alpha _a1 i1) c2          = unifyColored i1 c2    -- alpha value is ignored
unifyColored c1 (Alpha _a2 i2)          = unifyColored c1 i2
unifyColored c1 c2                      = (c1, c2)

unifyColorChannels      :: Channel ix -> Channel ix ->
                           ColorChannel ix -> ColorChannel ix -> (ColorChannel ix, ColorChannel ix)
unifyColorChannels _a1 a2 (Alpha a1 c11) c2
    | not (isAlpha c2)
        = let
          (c11', c2') = unifyColored c11 c2
          in
          (Alpha a1 c11', Alpha a2 c2')
unifyColorChannels a1 a2 c1 c2@(Alpha _a2 _c21)
    | not (isAlpha c1)
        = let
          (c1', c2') = unifyColorChannels a2 a1 c2 c2
          in
          (c2', c1')
unifyColorChannels _a1 _a2 c1 c2
    = unifyColored c1 c2

-- ----------------------------------------
--
-- basic ColorChannel function

-- overlay two ColorChannels, result is the most general ColorChannel required

overlayColorChannels    :: (ColorChannel ix -> ColorChannel ix -> ColorChannel ix)
overlayColorChannels c1@(Grey _)    _   = c1
overlayColorChannels c1@(RGB _ _ _) _   = c1
overlayColorChannels c1@(Alpha _ _) c2  = overlayAlpha c1 c2
    where
    overlayAlpha c1'@(Alpha a1 _i1) (Alpha a2 i2)
        = Alpha ra (overlayAlpha c1' i2)
          where
          ra = invertChannel (invertChannel a1 `multChannel` invertChannel a2)
    overlayAlpha (Alpha a1 i1) i2
        = uncurry (zipColored (mergeChannelsA a1)) (unifyColored i1 i2)         -- there are no alpha channels to be handled
    overlayAlpha _ _
        = error "overlayAlpha called with illegal arguments"

-- result of a mergeAlphaChannel is a ColorChannel without alpha part

mergeAlphaChannel       :: ColorChannel ix -> ColorChannel ix
mergeAlphaChannel (Alpha a1 i1)
                        = mapColored (a1 `multChannel`) i1
mergeAlphaChannel i1    = i1

addAlphaChannel         :: Channel ix -> ColorChannel ix -> ColorChannel ix
addAlphaChannel a (Alpha a1 i1)
                        = Alpha (a `multChannel` a1) i1
addAlphaChannel a c1    = Alpha a c1

addAlphaMask            :: (Ord ix, Num ix) =>
                           Geo ix -> ColorChannel ix -> ColorChannel ix
addAlphaMask g c        = addAlphaChannel (rectangleChannel g) c

toGreyChannel           :: ColorChannel ix -> ColorChannel ix
toGreyChannel           = foldColored Grey rgb2Grey alpha2Grey
                          where
                          rgb2Grey r g b = Grey $ addChannelSeq [r,g,b]
                          alpha2Grey a g = mergeAlphaChannel (Alpha a g)

toRGBChannel            :: ColorChannel ix -> ColorChannel ix
toRGBChannel            = foldColored greyToRgb RGB alpha2Rgb
                          where
                          greyToRgb x   = RGB x x x
                          alpha2Rgb a g = mergeAlphaChannel (Alpha a g)

-- ----------------------------------------

-- predefined channel values and combinators

uniChannel      :: Lightness -> Channel ix
uniChannel      = const . const

darkChannel     :: Channel ix
darkChannel     = uniChannel dark

lightChannel    :: Channel ix
lightChannel    = uniChannel light

addChannels     :: Channel ix -> Channel ix -> Channel ix
addChannels c1 c2 x y
                = (c1 x y + c2 x y) / 2

mergeChannels   :: Lightness -> Channel ix -> Channel ix -> Channel ix
mergeChannels a c1 c2
                = \ x y -> a * c1 x y + (1 - a) * c2 x y

mergeChannelsA  :: Channel ix -> Channel ix -> Channel ix -> Channel ix
mergeChannelsA a1 c1 c2 x y
                = a * c1 x y + (1 - a) * c2 x y
                  where
                  a = a1 x y

multChannel     :: Channel ix -> Channel ix -> Channel ix
multChannel     = merge (*)

invertChannel   :: Channel ix -> Channel ix
invertChannel   = ((1 -) `on2`)

addChannelSeq   :: [Channel ix] -> Channel ix
addChannelSeq cs
                = \ x y -> ( sum (map (\ c -> c x y) cs)
                             / fromIntegral (length cs)
                           )

rectangleChannel        :: (Num ix, Ord ix) => Geo ix -> Channel ix
rectangleChannel (Geo w h)
                = \ x y -> ( if 0 <= x && x < w && 0 <= y && y < h
                             then light
                             else dark
                           )

mkArray8Channel         :: Raster -> [Lightness] -> RasterChannel
mkArray8Channel (Geo w h) ls
    = at
    where
    toWord8     :: Lightness -> Word8
    toWord8 x   = toEnum (((floor (x * 256))::Int) `max` 0 `min` 255)

    word8array :: UArray Int Word8
    word8array = listArray (0, w * h - 1) (map toWord8 ls)

    at x y
       | 0 <= x && x < w &&
         0 <= y && y < h
           = fromIntegral (word8array ! (x + w * y)) / 255
       | otherwise
           = dark

mkArrayChannel          :: Raster -> [Lightness] -> RasterChannel
mkArrayChannel (Geo w h) ls
    = at
    where
    lightArray :: UArray Int Lightness
    lightArray = listArray (0, w * h - 1) ls

    at x y
       | 0 <= x && x < w &&
         0 <= y && y < h
           = lightArray ! (x + w * y)
       | otherwise
           = dark

channelToSeq            :: Raster -> RasterChannel -> [Lightness]
channelToSeq (Geo w h) f
    = [ f x y | y <- [0..h - 1], x <- [0..w - 1]]


raster8Channel  :: ([Lightness] -> [Lightness]) -> PixMap -> PixMap
raster8Channel rf = mapImageC raster
    where
    raster geo = mkArray8Channel geo . rf . channelToSeq geo

raster8PixMap   :: PixMap -> PixMap
raster8PixMap   = raster8Channel id

rasterPixMap    :: PixMap -> PixMap
rasterPixMap    = mapImageC raster
    where
    raster geo = mkArrayChannel geo . channelToSeq geo

-- ----------------------------------------
--
-- predefined geometry values anf functions

instance Show ix => Show (Geo ix) where
    show (Geo w h) = show w ++ "x" ++ show h

mkGeo   :: (Num ix, Ord ix, Show ix) => ix -> ix -> Geo ix
mkGeo w h
    | w > 0 && h > 0    = Geo w h
    | otherwise         = error $ "illegal geometry " ++ show (Geo w h)

maxGeo          :: Ord ix => Geo ix -> Geo ix -> Geo ix
(Geo w1 h1) `maxGeo` (Geo w2 h2)
                = Geo (w1 `max` w2) (h1 `max` h2)

minGeo          :: Ord ix => Geo ix -> Geo ix -> Geo ix
(Geo w1 h1) `minGeo` (Geo w2 h2)        = Geo (w1 `min` w2) (h1 `min` h2)

flipGeo         :: Geo ix -> Geo ix
flipGeo (Geo w h)
                = Geo h w

scaleGeo        :: (Ord ix, Num ix, Show ix) => ix -> ix -> Geo ix -> Geo ix
scaleGeo n m (Geo w h)
                = mkGeo (n * w) (m * h)

shiftGeo        :: (Ord ix, Num ix, Show ix) => ix -> ix -> Geo ix -> Geo ix
shiftGeo n m (Geo w h)
                = mkGeo (w + n) (h + m)

partGeo         :: (Integral ix, Show ix) => ix -> ix -> Geo ix -> Geo ix
partGeo n m (Geo w h)
                = mkGeo ((w + n - 1) `div` n) ((h + m - 1) `div` m)

resizeGeo       :: (Num ix, Ord ix, Show ix) =>
                   ix -> ix -> Geo ix -> Geo ix
resizeGeo n m (Geo w h)
    = mkGeo w1 h1
    where
    w1 | n == 0    = w
       | otherwise = n
    h1 | m == 0    = h
       | otherwise = m

-- ----------------------------------------
--
-- basic Image functions

mapImage        :: (Geo ix1 -> Geo ix2) ->
                   (Geo ix1 -> a -> b) ->
                   (Image ix1 a -> Image ix2 b)
mapImage mapg mapi (Image g i)
                = Image (mapg g) (mapColored (mapi g) i)

-- mapImageC remains geoemtry unchanged, but transforms all channels including alpha

mapImageC       :: (Geo ix -> a -> b) ->
                   (Image ix a -> Image ix b)
mapImageC       = mapImage id

-- mapImageCA does not change alpha channel or is the geometry changed

mapImageCA      :: (a -> a) ->
                   (Image ix a -> Image ix a)
mapImageCA mapi (Image g i)
                = Image g (mapColored2 id mapi i)

-- transfLight changes all color values, but remains alpha unchanged

transfLight     :: (Lightness -> Lightness) ->
                   (ImgMap ix -> ImgMap ix)
transfLight tf  = mapImageCA (tf `on2`)

-- ----------------------------------------
--
-- functions defined on all images

class Channels a where
    red         :: a c -> c
    green       :: a c -> c
    blue        :: a c -> c
    lumi        :: a c -> c
    alpha       :: a c -> c

    isGrey      :: a c -> Bool
    isGrey      = const False

    isRGB       :: a c -> Bool
    isRGB       = const False

    isAlpha     :: a c -> Bool
    isAlpha     = const False

    isRGBA      :: a c -> Bool
    isRGBA i    = isRGB i && isAlpha i

instance Channels Colored where
    red         (Grey x)        = x
    red         (RGB r _ _)     = r
    red         (Alpha _ i)     = red i

    green       (Grey x)        = x
    green       (RGB _ g _)     = g
    green       (Alpha _ i)     = green i

    blue        (Grey x)        = x
    blue        (RGB _ _ b)     = b
    blue        (Alpha _ i)     = blue i

    lumi        (Grey x)        = x
    lumi        (RGB _ _ _)     = error "lumi called for RGB"
    lumi        (Alpha _ i)     = lumi i

    alpha       (Grey _)        = error "alpha called for Grey"
    alpha       (RGB _ _ _)     = error "alpha called for RGB"
    alpha       (Alpha a _)     = a


    isGrey      (Grey _)        = True
    isGrey      (Alpha _ i)     = isGrey i
    isGrey      _               = False

    isRGB       (RGB _ _ _)     = True
    isRGB       (Alpha _ i)     = isRGB i
    isRGB  _                    = False

    isAlpha     (Alpha _ _)     = True
    isAlpha  _                  = False

instance Channels (Image ix) where
    red         = red     . imgCol
    green       = green   . imgCol
    blue        = blue    . imgCol
    lumi        = lumi    . imgCol
    alpha       = alpha   . imgCol
    isGrey      = isGrey  . imgCol
    isRGB       = isRGB   . imgCol
    isAlpha     = isAlpha . imgCol

-- ----------------------------------------

mkBitMap        :: Raster -> (Int -> Int -> Bool) -> PixMap
mkBitMap geo at
    = mkGrey geo ((fromIntegral . fromEnum) `on2` at)

mkGrey          :: Geo ix -> a -> Image ix a
mkGrey geo at
    = Image geo (Grey at)

mkRGB           :: Geo ix -> a -> a -> a -> Image ix a
mkRGB geo r g b
    = Image geo (RGB r g b)

addAlpha        :: Channel ix -> ImgMap ix -> ImgMap ix
addAlpha ac (Image geo i)
    = Image geo (addAlphaChannel ac i)

mergeAlpha      :: ImgMap ix -> ImgMap ix
mergeAlpha i@(Image g c)
    | isAlpha i = Image g $ mergeAlphaChannel c
    | otherwise = i

-- ----------------------------------------

readImage       :: String -> PixMap
readImage       = readPNM . item

readPNM         :: (String, String) -> PixMap
readPNM ("P1", str)
    = mkBitMap (mkGeo w h) at
      where
      (ns, rest)= readRow 2 str
      [w, h]    = map read ns
      pixels    = fst . readRow len $ rest
      len       = w * h

      bits      :: [Int]
      bits      = map read pixels

      bitMx     :: UArray Int Bool
      bitMx     = listArray (0, len - 1) . map (== 0) $ bits

      at x y
          | 0 <= x && x < w &&
            0 <= y && y < h
                =  bitMx ! (x + w * y)
          | otherwise
              = False

readPNM ("P2", str)
    = mkGrey (mkGeo w h) at
      where
      ([w', h', m'], rest)      = readRow 3 str
      w         = (read w')::Int
      h         = (read h')::Int
      m         = (read m')::Lightness
      l         = w * h
      pixels    = fst . readRow l $ rest

      bytes     :: [Word8]
      bytes     = map read pixels

      byteMx    :: UArray Int Word8
      byteMx    = listArray (0, l - 1) bytes

      at x y
          | 0 <= x && x < w &&
            0 <= y && y < h
                =  fromIntegral (byteMx ! (x + w * y)) / m
          | otherwise
              = dark

readPNM ("P3", str)
    = mkRGB (mkGeo w h) (at 0) (at 1) (at 2)
      where
      ([w', h', m'], rest)      = readRow 3 str
      w         = (read w')::Int
      h         = (read h')::Int
      m         = (read m')::Lightness
      len       = w * h * 3
      pixels    = fst . readRow len $ rest

      bytes     :: [Int]
      bytes     = map read pixels

      byteMx    :: UArray Int Int
      byteMx    = listArray (0, len - 1) bytes

      at c x y
          | 0 <= x && x < w &&
            0 <= y && y < h
                =  fromIntegral (fromEnum (byteMx ! (c + 3 * (x + w * y)))) / m
          | otherwise
              = dark

readPNM ("P5", str)
    = mkGrey (mkGeo w h) at
      where
      ([w', h', m'], rest)      = readRow 3 str
      w         = (read w')::Int
      h         = (read h')::Int
      m         = (read m')::Lightness
      l         = w * h

      bytes     :: [Word8]
      bytes     = map (toEnum . fromEnum) .tail $ rest

      byteMx    :: UArray Int Word8
      byteMx    = listArray (0, l - 1) bytes

      at x y
          | 0 <= x && x < w &&
            0 <= y && y < h
                =  fromIntegral (byteMx ! (x + w * y)) / m
          | otherwise
              = dark

readPNM ("P6", str)
    = mkRGB (mkGeo w h) (at 0) (at 1) (at 2)
      where
      ([w', h', m'], rest)      = readRow 3 str
      w         = (read w')::Int
      h         = (read h')::Int
      m         = (read m')::Lightness
      len       = w * h * 3

      bytes     :: [Int]
      bytes     = map (toEnum . fromEnum) . tail $ rest

      byteMx    :: UArray Int Int
      byteMx    = listArray (0, len - 1) bytes

      at c x y
          | 0 <= x && x < w &&
            0 <= y && y < h
                =  fromIntegral (fromEnum (byteMx ! (c + 3 * (x + w * y)))) / m
          | otherwise
              = dark

readPNM (fmt, _)
    = error $ "unsupported PNM format " ++ show fmt

-- ----------------------------------------

item    :: String -> (String, String)
item str
    = i1
      where
      i@(w,r) = head (lex str)
      i1 | head w == '#'
             = item r1
         | otherwise
             = i
         where
         r1 = ( drop 1 . snd . break (== '\n') . drop 1) r

readFct :: (String -> (a, String)) ->
           Int ->
           String -> ([a], String)

readFct _f 0 str
    = ([],str)

readFct f n str
    = (i:r, str2)
      where
      (i, str1) = f str
      (r, str2) = readFct f (n-1) str1

readRow         :: Int -> String -> ([String], String)
readRow         = readFct item

-- ----------------------------------------

showImage       :: Bool -> PixMap -> String
showImage bin (Image geo@(Geo w h) i)
    = imgType i1 ++ imgG ++ "255\n" ++ imgData i1
    where
    i1   = mergeAlphaChannel i
    imgG = show w ++ " " ++ show h ++ "\n# Haskell PPM Tools\n"

    imgType     :: ColorChannel ix -> String
    imgType (Grey _)
        | bin           = "P5\n"
        | otherwise     = "P2\n"
    imgType _
        | bin           = "P6\n"
        | otherwise     = "P3\n"

    imgData     :: ColorChannel Int -> String
    imgData
        = pixToChar bin .
          map toPix .
          foldColored id mergePixs undefined {- mergeA -} .
          mapColored (pixToList geo)

    mergePixs   :: [Lightness] -> [Lightness] -> [Lightness] -> [Lightness]
    mergePixs (x:xs) (y:ys) (z:zs) = x : y : z : mergePixs xs ys zs
    mergePixs  _      _      _     = []

    -- mergeA   :: [Lightness] -> [Lightness] -> [Lightness]
    -- mergeA (a:as) (r:g:b:xs) = (a*r) : (a*g) : (a*b) : mergeA as xs
    -- mergeA _     _           = []

    pixToChar   :: Bool -> [Int] -> String
    pixToChar b
        | b             = map toEnum
        | otherwise     = concatMap ((++ "\n") . show)

    toPix       :: Lightness -> Int
    toPix c = floor (c * 256.0) `min` 255 `max` 0

pixToList       :: Raster -> RasterChannel -> [Lightness]
pixToList (Geo w h) f
                = [ f x y | y <- [0..h-1], x <- [0..w-1] ]

-- ----------------------------------------

readImageFile   :: FilePath -> IO PixMap
readImageFile src0
    = readF [takeExtension src0, ".ppm", ".pgm"] src0
      where
      readF     :: [String] -> FilePath -> IO PixMap
      readF [] src
          = return (error $ "file not found: " ++ show src)
      readF (e1:es) src
          = do
            ex <- doesFileExist src'
            if ex
               then do
                    h <- openFile src' ReadMode
                    c <- B.hGetContents h
                    hClose h
                    return (readImage (C.unpack c))
               else readF es src
          where
          src' = replaceExtension src e1

writeImageFile  :: FilePath -> PixMap -> IO ()
writeImageFile dst img
    = B.writeFile dst' (C.pack (showImage True img))
      where
      dst'      = replaceExtension dst (defaultExt ext)
      ext       = takeExtension $ dst
      defaultExt e
          | e `elem` ["", "pgm"] && isGrey img =      ".pgm"
          | e `elem` ["", "ppm"] && isRGB  img =      ".ppm"
          |                         isGrey img = e ++ ".pgm"
          | otherwise                          = e ++ ".ppm"

-- ----------------------------------------

expandImage     :: (Num ix, Ord ix) =>
                   Geo ix -> ImgMap ix -> ImgMap ix
expandImage g1 (Image g0 i0)
    = Image g1 (addAlphaMask g0 i0)

equalWidth      :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix -> (ImgMap ix, ImgMap ix)
equalWidth i1@(Image (Geo w1 h1) _c1) i2@(Image (Geo w2 h2) _c2)
    | w1 <  w2  = (expandImage (Geo w2 h1) i1, i2)
    | w1 >  w2  = (i1, expandImage (Geo w1 h2) i2)
    | otherwise = (i1, i2)

equalHeight     :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix -> (ImgMap ix, ImgMap ix)
equalHeight i1@(Image (Geo w1 h1) _c1) i2@(Image (Geo w2 h2) _c2)
    | h1 <  h2  = (expandImage (Geo w1 h2) i1, i2)
    | h1 >  h2  = (i1, expandImage (Geo w2 h1) i2)
    | otherwise = (i1, i2)

equalSize       :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix -> (ImgMap ix, ImgMap ix)
equalSize i1    = uncurry equalHeight . equalWidth i1


unifyImages     :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix -> (ImgMap ix, ImgMap ix)
unifyImages (Image g1 c1) (Image g2 c2)
                = (Image g1 c1', Image g2 c2')
                  where
                  (c1', c2') = unifyColorChannels (rectangleChannel g1) (rectangleChannel g2) c1 c2


above           :: (Num ix, Ord ix, Show ix) =>
                   ImgMap ix -> ImgMap ix -> ImgMap ix
above i1 i2     = uncurry above' . uncurry unifyImages . equalWidth i1 $ i2
                  where
                  above' (Image (Geo w1 h1) c1) (Image (Geo _w2 h2) c2)
                      = Image (mkGeo w1 (h1 + h2)) $ zipColored above'' c1 c2
                        where
                        above'' f1 f2
                            = \ x y -> (if y < h1 then f1 x y else f2 x (y - h1))

aboves          :: (Num ix, Ord ix, Show ix) =>
                   [ImgMap ix] -> ImgMap ix
aboves          = foldr1 above

sideBySide              :: (Num ix, Ord ix, Show ix) =>
                   ImgMap ix -> ImgMap ix -> ImgMap ix
sideBySide i1   = uncurry side' . uncurry unifyImages . equalHeight i1
                  where
                  side' (Image (Geo w1 h1) c1) (Image (Geo w2 _h2) c2)
                      = Image (mkGeo (w1 + w2) h1) $ zipColored side'' c1 c2
                        where
                        side'' f1 f2
                            = \ x y -> (if x < w1 then f1 x y else f2 (x - w1) y)

sideBySides     :: (Num ix, Ord ix, Show ix) =>
                   [ImgMap ix] -> ImgMap ix
sideBySides     = foldr1 sideBySide

{- old ineficcent versions: to many alpha channels

above           :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix -> ImgMap ix
above i1@(Image (Geo _w1 h1) _c1) i2
                = overlay (addMask i1) (shift 0 h1 i2)

sideBySide      :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix -> ImgMap ix
sideBySide i1@(Image (Geo w1 _h1) _c1) i2
    = overlay (addMask i1) (shift w1 0 i2)

-}

-- ----------------------------------------

melt            :: (Ord ix, Num ix) =>
                   (Lightness -> Lightness -> Lightness) ->
                   ImgMap ix -> ImgMap ix -> ImgMap ix
melt op i1 i2   = uncurry merge' .
                  unifySizeColor i1 $ i2
                  where
                  merge' (Image g1 c1) (Image _g2 c2)
                      = Image g1 $ zipColored (merge op) c1 c2

unifySizeColor  :: (Ord ix, Num ix) =>
                   ImgMap ix -> ImgMap ix -> (ImgMap ix, ImgMap ix)
unifySizeColor  i1 i2
                = uncurry unifyImages .
                  ( \ (i1', i2') -> (mergeAlpha i1', mergeAlpha i2') ) .
                  equalSize i1 $ i2

-- ----------------------------------------


invert  :: ImgMap ix -> ImgMap ix
invert  = transfLight invertLight

gamma   :: Lightness -> ImgMap ix -> ImgMap ix
gamma   = transfLight . gammaLight

bitmap  :: ImgMap ix -> ImgMap ix
bitmap  = transfLight lightOrDark

reduce  :: Int -> ImgMap ix -> ImgMap ix
reduce  = transfLight . reduceLight

bitmap1 :: PixMap -> PixMap
bitmap1 = raster8Channel rasterize
    where
    rasterize   :: [Lightness] -> [Lightness]
    rasterize   = snd . mapAccumL zOo 0.5
                where
                zOo acc x
                    | acc' >= 1 = (1 - acc', 1)
                    | otherwise = (acc', 0)
                    where
                    acc' = acc + x

-- ----------------------------------------

flipV           :: PixMap -> PixMap
flipV           = mapImageC $ \ geo f x y -> f (width geo - x - 1) y

flipH           :: PixMap -> PixMap
flipH           = mapImageC $ \ geo f x y -> f x (height geo - y - 1)

flipD           :: PixMap -> PixMap
flipD           = mapImage flipGeo $ \ _geo f x y -> let d = x - y in f (x - d) (y + d)

rot90           :: PixMap -> PixMap
rot90           = mapImage flipGeo $ \ geo f x y -> f y (width geo - x - 1)

rot180          :: PixMap -> PixMap
rot180          = flipV . flipH

rot270          :: PixMap -> PixMap
rot270          = rot90 . rot180


shiftRot        :: Int -> Int -> PixMap -> PixMap
shiftRot n m    = mapImageC $
                  \ (Geo w h) f x y -> f ((x - n) `mod` w) ((y - m) `mod` h)

tile            :: Int -> Int -> PixMap -> PixMap
tile n m        = mapImage (scaleGeo n m) $
                  \ (Geo w h) f x y -> f (x `mod` w) (y `mod` h)

tileMirr        :: Int -> Int -> PixMap -> PixMap
tileMirr n m    = mapImage (scaleGeo n m) $
                  \ (Geo w h) f x y
                      -> let
                         fw = f1 w
                         fh = f1 h
                         in
                         f (fw x) (fh y)
                  where
                  f1 l z
                      | even zd   = zm
                      | otherwise = l - zm - 1
                      where
                      zd = z `div` l
                      zm = z `mod` l

scale           :: Int -> Int -> PixMap -> PixMap
scale 1 1       = id
scale n m       = mapImage (scaleGeo n m) $
                  \ _geo f x y -> f (x `div` n) (y `div` m)

shrink          :: Int -> Int -> PixMap -> PixMap
shrink 1 1      = id
shrink n m      = rasterPixMap . mapImage (partGeo n m) shrink'
                  where
                  shrink' (Geo w h) f x y
                      = sum [ f (x' `min` (w - 1)) (y' `min` (h - 1)) |
                              x' <- [ n * x .. n * (x + 1) - 1 ],
                              y' <- [ m * y .. m * (y + 1) - 1 ]
                            ]
                        / fromIntegral (n * m)
                              
shift           :: (Num ix, Ord ix, Show ix) =>
                   ix -> ix -> ImgMap ix -> ImgMap ix
shift w h
                = mapImage (shiftGeo w h) $
                  \ _geo f x y -> f (x - w) (y - h)


cut             :: (Num ix, Ord ix) =>
                   Geo ix -> ImgMap ix -> ImgMap ix
cut g1 i1@(Image g0 i)
    | g1 == g0  = i1
    | otherwise = Image g (addAlphaMask g i)
                  where
                  g = g1 `minGeo` g0

overlay         :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix -> ImgMap ix
overlay (Image g1 i1) (Image g2 i2)
                = Image (g1 `maxGeo` g2) $
                  overlayColorChannels i1 i2

addMask         :: (Num ix, Ord ix) =>
                   ImgMap ix -> ImgMap ix
addMask (Image g1 i1)
                = Image g1 (addAlphaMask g1 i1)

-- ----------------------------------------

meanX           :: PixMap -> PixMap
meanX           = mapImage (partGeo 2 1) mean'
                  where
                  mean' (Geo w _h) f x y
                      = (f x1 y + f x2 y) / 2.0
                      where
                      x1 = 2 * x - 1
                      x2 = 2 * x `min` (w - 1)

meanY           :: PixMap -> PixMap
meanY           = mapImage (partGeo 1 2) mean'
                  where
                  mean' (Geo _w h) f x y
                      = (f x y1 + f x y2) / 2.0
                      where
                      y1 = 2 * y - 1
                      y2 = 2 * y `min` (h - 1)

meanXY          :: PixMap -> PixMap
meanXY          = meanY . meanX

diffX           :: PixMap -> PixMap
diffX           = mapImage (partGeo 2 1) diff'
                  where
                  diff' (Geo w _h) f x y
                      = (f x1 y - f x2 y + 1.0) / 2.0
                      where
                      x1 = 2 * x - 1
                      x2 = 2 * x `min` (w - 1)

diffY           :: PixMap -> PixMap
diffY           = mapImage (partGeo 1 2) diff'
                  where
                  diff' (Geo _w h) f x y
                      = (f x y1 - f x y2 + 1.0) / 2.0
                      where
                      y1 = 2 * y - 1
                      y2 = 2 * y `min` (h - 1)

diffXY          :: PixMap -> PixMap
diffXY          = diffY . diffX

partX           :: Int -> PixMap -> [PixMap]
partX n img     = map part [0 .. n-1]
                  where
                  part i = mapImage (partGeo n 1) (const partImg) img
                           where
                           partImg f x y
                               = f (n * x + i) y

partY           :: Int -> PixMap -> [PixMap]
partY n img     = map part [0 .. n-1]
                  where
                  part i = mapImage (partGeo 1 n) (const partImg) img
                           where
                           partImg f x y
                               = f x (n * y + i)

splitX          :: Int -> PixMap -> [PixMap]
splitX n img    = map (rasterPixMap . split) [0 .. n-1]
                  where
                  split i = mapImage splitGeo splitImg img
                           where
                           splitGeo = partGeo n 1
                           splitImg geo f x y
                               = f (w1 * i + x) y
                               where
                               (Geo w1 _) = splitGeo geo

splitY          :: Int -> PixMap -> [PixMap]
splitY n img    = map (rasterPixMap . split) [0 .. n-1]
                  where
                  split i = mapImage splitGeo splitImg img
                           where
                           splitGeo = partGeo 1 n
                           splitImg geo f x y
                               = f x (h1 * i + y)
                               where
                               (Geo _ h1) = splitGeo geo

resizeX         :: Int -> PixMap -> PixMap
resizeX w1 img@(Image (Geo w0 _) _)
    | w0 == w1  = img
    | otherwise = rasterPixMap . mapImage (resizeGeo w1 0) resize $ img
                  where
                  resize (Geo w _h) f x y
                      = (1 - a) * f x0' y + a * f x1' y
                      where
                      r :: Double
                      r = fromIntegral ( (w - 1) `max` 1) / fromIntegral ( (w1 - 1) `max` 1)
                      x' :: Double
                      x' = fromIntegral x * r
                      x0', x1' :: Int
                      x0' = floor x'
                      x1' = x0' + 1
                      a  :: Double
                      a  = x' - fromIntegral x0'

resizeY         :: Int -> PixMap -> PixMap
resizeY h1 img@(Image (Geo _ h0) _)
    | h0 == h1  = img
    | otherwise = rasterPixMap . mapImage (resizeGeo 0 h1) resize $ img
                  where
                  resize (Geo _w h) f x y
                      = (1 - a) * f x y0' + a * f x y1'
                      where
                      r :: Double
                      r = fromIntegral ( (h - 1) `max` 1) / fromIntegral ( (h1 - 1) `max` 1)
                      y' :: Double
                      y' = fromIntegral y * r
                      y0', y1' :: Int
                      y0' = floor y'
                      y1' = y0' + 1
                      a  :: Double
                      a  = y' - fromIntegral y0'

mergeX          :: PixMap -> PixMap -> PixMap
mergeX i1 i2    = uncurry mergei (unifySizeColor i1 i2)
                  where
                  mergei (Image (Geo w1 h1) c1) (Image (Geo w2 _h2) c2)
                      = Image (mkGeo (w1 + w2) h1) $ zipColored merge' c1 c2
                        where
                        merge' f1 f2
                            = \ x y -> (if even x then f1 else f2) (x `div` 2) y

mergeY          :: PixMap -> PixMap -> PixMap
mergeY i1 i2    = uncurry mergei (unifySizeColor i1 i2)
                  where
                  mergei (Image (Geo w1 h1) c1) (Image (Geo _w2 h2) c2)
                      = Image (mkGeo w1 (h1 + h2)) $ zipColored merge' c1 c2
                        where
                        merge' f1 f2
                            = \ x y -> (if even y then f1 else f2) x (y `div` 2)

-- ----------------------------------------
{-

minLightness            :: (FoldImage img) => ImgWithGeo img -> Lightness
minLightness (IWG g i)
                        = foldlImage g min (2.0) i

maxLightness            :: (FoldImage img) => ImgWithGeo img -> Lightness
maxLightness (IWG g i)
                        = foldlImage g max (-1.0) i

rngLightness            :: (FoldImage img) => ImgWithGeo img -> (Lightness, Lightness)
rngLightness (IWG g i)
                        = foldlImage g minMax (2.0,-1.0) i
                          where
                          minMax (mi, ma) x = (mi `min` x, ma `max` x)

meanLightness           :: (Image img, FoldImage img) => ImgWithGeo img -> Lightness
meanLightness (IWG g i)
                        = foldlImage g (+) 0.0 i
                          / fromIntegral ( width g * height g *
                                           if isRGB i
                                           then 3
                                           else 1
                                         )

-- ----------------------------------------
-}