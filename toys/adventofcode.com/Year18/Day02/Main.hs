-- solution for
-- http://adventofcode.com/2018/day/2


module Main where

import           Util.Main1 (main12)
import qualified Data.IntMap.Strict as M
import           Data.Monoid
import           Data.List (sort)
import           Control.Arrow ((***))

-- ----------------------------------------

main :: IO ()
main = main12 "2018-01"
       day02 captcha1
       day02 captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = solve2 . fromString

-- ----------------------------------------

type    Count     = Sum Int

newtype CharCount = CC (M.IntMap Count)

instance Semigroup CharCount where
  CC m1 <> CC m2 = CC $ M.unionWith (<>) m1 m2

instance Monoid CharCount where
  mempty = CC M.empty

foldMapCC :: Monoid m => (Count -> m) -> CharCount -> m
foldMapCC f (CC m) = foldMap f m

singleCC :: Char -> CharCount
singleCC c = CC $ M.singleton (fromEnum c) (Sum 1)

solve1 :: [String] -> Int
solve1 = uncurry (*) . (getSum *** getSum) . mconcat . map countWord
  where
    countWord :: String -> (Count, Count)
    countWord = toP . foldMapCC toAny . foldMap singleCC
      where
        toAny :: Count -> (Any, Any)
        toAny (Sum x)
          | x == 2    = (Any True, mempty)
          | x == 3    = (mempty, Any True)
          | otherwise = mempty

        toP :: (Any, Any) -> (Count, Count)
        toP = (toC *** toC)
          where
            toC = Sum . fromEnum . getAny

-- ----------------------------------------

solve2 :: [String] -> String
solve2 ws = maybe "" id $ diffAll (length $ head ws) ws

rotate :: [String] -> [String]
rotate = map (\ (x : xs) -> xs ++ [x])

rotateBack :: String -> String
rotateBack [] = []
rotateBack xs = last xs : init xs

diffFst :: [String] -> Maybe String
diffFst = dup . sort . map tail
  where
    dup (w1 : l2@(w2 : _))
      | w1 == w2 = Just w1
      | otherwise = dup l2
    dup _ = Nothing

diffAll :: Int -> [String] -> Maybe String
diffAll 0 _ws = Nothing
diffAll n  ws =
  diffFst ws
  <>
  (rotateBack  <$> diffAll (n - 1) (rotate ws))

fromString :: String -> [String]
fromString = lines

day01' :: IO String
day01' = readFile "./Year18/Day01/day01.txt"

ex1 :: String
ex1 = unlines
  [ "abcdef"
  , "bababc"
  , "abbcde"
  , "abcccd"
  , "aabcdd"
  , "abcdee"
  , "ababab"
  ]

ex2 :: String
ex2 = unlines
  [ "abcde"
  , "fghij"
  , "klmno"
  , "pqrst"
  , "fguij"
  , "axcye"
  , "wvxyz"
  ]

day02 :: String
day02 = unlines
  [ "rmyxgdlihczskunpfwbgqoeybv"
  , "rmyxgdlksczskunpfwbjqkeatv"
  , "rmybgdxibczskunpfwbjqoeatv"
  , "rmyxgdlirczskuopfwbjqzeatv"
  , "rmyxedlrhczskunpfwbyqoeatv"
  , "rmyxfdlicczskunpfwbxqoeatv"
  , "rmyxgvlihkzskunpfwbsqoeatv"
  , "rmyxgdaihczvkunpfwblqoeatv"
  , "nmyxgolihczskunpfwbjqieatv"
  , "rhyxgdcihczskunifwbjqoeatv"
  , "rmfxgdlihczskunpfwbvqgeatv"
  , "smyxgdlihczskunsiwbjqoeatv"
  , "rmyxgdcihcxskunpfwbrqoeatv"
  , "rmyxgdlihczckuiqfwbjqoeatv"
  , "rmyxxdwihczskunifwbjqoeatv"
  , "rkzxgdlihczskunpfwhjqoeatv"
  , "rmypgdlihczskunpfwbrqoeafv"
  , "rmyxgplihczvkunpkwbjqoeatv"
  , "rqyxgdlihdzskjnpfwbjqoeatv"
  , "rmyxgdlihczskqnpswbjqoeaov"
  , "mcyxgdlihczmkunpfwbjqoeatv"
  , "rmyxgdlohczspunpowbjqoeatv"
  , "tmyxgdlihczskunpfwbeqoeltv"
  , "rmyxgdlibccskunpfwbjqoegtv"
  , "rmyxgdlehczsaunpfwboqoeatv"
  , "rmaxgdlihczseunpfwbjqojatv"
  , "rmyxgdlijczskynpfwbjboeatv"
  , "kmlxgdlilczskunpfwbjqoeatv"
  , "rmsxgdlshczskenpfwbjqoeatv"
  , "rmbxgdlihcmskgnpfwbjqoeatv"
  , "rayxgdlihczskunpfwbjqoeaef"
  , "umyxgdlisczskunpfdbjqoeatv"
  , "rmyxgdlihczskunsfwbjqieatg"
  , "rmbxgdlihczhkunpfwbjqoeamv"
  , "rmyxgdlihczskeypfwbjqxeatv"
  , "rmyxgkrihczskunptwbjqoeatv"
  , "rmyxgdlihczskunpawbjqoexiv"
  , "rmyxgdlihcrskqnpfwbjqceatv"
  , "rmyxgblihczskjnpfwbjqieatv"
  , "rmyggdlidczskunofwbjqoeatv"
  , "rmyxgdlghczskunphwbjqomatv"
  , "rmqxgdbihczskunpfnbjqoeatv"
  , "rvyxgdlihczsgunpfwbjqoeanv"
  , "royxgdlnhczskqnpfwbjqoeatv"
  , "rmyxgdlihczskugpfwbkqreatv"
  , "rmyxfdlihczskunppwejqoeatv"
  , "rqyxgdlipczskunpfwbjqoeqtv"
  , "rmyxgdlicczskunpnwbjqotatv"
  , "rmyxodlihczskxnpfwijqoeatv"
  , "rmyxrdyihczskunpftbjqoeatv"
  , "rmtxgdyihwzskunpfwbjqoeatv"
  , "tmyxcdliiczskunpfwbjqoeatv"
  , "rmyxgdlihczskmnpfwbjjoeadv"
  , "rmyxgdnihczskunpqwbjqojatv"
  , "bmyxgdlihczskcnpfwboqoeatv"
  , "rmysgdlihcyskudpfwbjqoeatv"
  , "rmyxgdtihczsmuupfwbjqoeatv"
  , "rmyxgdlihczssunpffbjqolatv"
  , "rmyogdlihczsklnpfwbjqoxatv"
  , "rmyxgjlihczskunpfwsjqoyatv"
  , "rmyxgalshczskunpfwbuqoeatv"
  , "rmyfgdlihczskunqfwbiqoeatv"
  , "tmyxgdlihczskunotwbjqoeatv"
  , "rmyxpdzihczskuopfwbjqoeatv"
  , "rmyfgdlihczskunpfrbgqoeatv"
  , "rmyxgdlwhczskhnofwbjqoeatv"
  , "rmyxgdlihczsmudpfrbjqoeatv"
  , "rmyxgdlihczokanpfwbjqooatv"
  , "rmyxrdlihczskunppwjjqoeatv"
  , "rmyxgdjihczskwnpowbjqoeatv"
  , "mmyxgdlihczikunpfwbjqoeamv"
  , "rmyxgflihczshunpwwbjqoeatv"
  , "rmytghlihczskunpfwbjqoeatk"
  , "rmyxgdlipczmbunpfwbjqoeatv"
  , "rmyxgdlihczkkonpfwbjqomatv"
  , "rmfxgslihczskunpfwujqoeatv"
  , "dmyxgdlihczykunqfwbjqoeatv"
  , "rmyxgalihcbskunpgwbjqoeatv"
  , "rmyxgdlinczqkunpfwbjqopatv"
  , "rmyxgdlihwzslunplwbjqoeatv"
  , "rmypgdlihczskdtpfwbjqoeatv"
  , "rmsxgdxieczskunpfwbjqoeatv"
  , "rmyxgdlihczskwnpfxrjqoeatv"
  , "rmyxgdlihzzskunpflbjpoeatv"
  , "rslxgdlihczsnunpfwbjqoeatv"
  , "rmyxgdlmcczskunpfwbjqoealv"
  , "fmkxgdbihczskunpfwbjqoeatv"
  , "rmyxgdiigczxkunpfwbjqoeatv"
  , "rjyxgnlqhczskunpfwbjqoeatv"
  , "ymyxgolihczskunpfmbjqoeatv"
  , "hmyxgdlihczskuncfwbjqoejtv"
  , "rmyxgqlihczzkunpfwbjqojatv"
  , "rmgfgdlihczskunpfwbjgoeatv"
  , "rmyxgdlfhczskunpfwbjqweaxv"
  , "rmoxtdlihczskunpfwdjqoeatv"
  , "ruyxgdlihczskunpfmbjnoeatv"
  , "rmnxgflehczskunpfwbjqoeatv"
  , "rmyugdlihczskunpfwfjroeatv"
  , "rmyxddbihczskunpfwbjqoeutv"
  , "rmyxgdlipczskunofbbjqoeatv"
  , "gmyxgdlihczskunpfkbjroeatv"
  , "rmyxgdllhcpskunpfwbjqqeatv"
  , "rmyxgdlihchskunpfwbjqoelcv"
  , "mmyxldlihczskuncfwbjqoeatv"
  , "ryyxgdlxhczskcnpfwbjqoeatv"
  , "rmyxpdlihczskyntfwbjqoeatv"
  , "rmhxgdlibczskwnpfwbjqoeatv"
  , "rmyxgdlihczskunpfwojbkeatv"
  , "qmyxgdlihczskunpfwbjqoyatm"
  , "rmyxgdlzhczskunpfwbjqoealr"
  , "rmyegdliqczskunpfgbjqoeatv"
  , "umyxgdlihczsvunpfwbfqoeatv"
  , "rmyxgdoihfzskunpfmbjqoeatv"
  , "rmyxgdlihcdskanpmwbjqoeatv"
  , "rmyxgdyihczskunpfrbjqoeaov"
  , "rcyxgdlihczskuegfwbjqoeatv"
  , "rmyxgdlihgwskunpfwbjkoeatv"
  , "rpyxgdlihmzskunpfwbjqoeatp"
  , "rmyxgdlihhzskunpfwbjaoeapv"
  , "rmyxgdsrhczskunpflbjqoeatv"
  , "rmrxgdlihczskunpvwbjqoeabv"
  , "rmcxgylihczskunpfwbjyoeatv"
  , "rmkxgdlyhczsounpfwbjqoeatv"
  , "rmyxgdqihczskunmfwbjqoratv"
  , "rmyxgdlihczskunpfibjqofath"
  , "rmyxgdliqczskunpqwbjqoeaev"
  , "rmhxgdlizcjskunpfwbjqoeatv"
  , "rmyxgdlfhcwskunpfwbjqoeaqv"
  , "rmyxgdlchclskunpfwbdqoeatv"
  , "rmyxgdluhczswunpfwbjqoeatt"
  , "rmyxgdlzqczskunpfwbjqoeatq"
  , "rmdxgdlihszskunpfwbwqoeatv"
  , "rmyxgdlihszsvunpfwbjqueatv"
  , "rmyxgdlhhczskunpffbjaoeatv"
  , "rmrxgdlphczskunpfwbjqreatv"
  , "hmyngdxihczskunpfwbjqoeatv"
  , "rmyxgdlizczpkunpfwbyqoeatv"
  , "rmyxbdlihyzskunlfwbjqoeatv"
  , "rmyxgdlipczsqunnfwbjqoeatv"
  , "rmyxgdlihcsskunpfxbjqoaatv"
  , "rmyxgdljhcznkunpfwbjqfeatv"
  , "rmaxgdlihczspunpfwbjqoqatv"
  , "rsyxgdlihczskunpfwbjqoehcv"
  , "rmyxgjlicczskunpfwbjqoeitv"
  , "rwymgvlihczskunpfwbjqoeatv"
  , "rmyxgdlipfzskunpfwbjqweatv"
  , "rmyxgglihczskunpgwbjqoealv"
  , "royxgdlihczskhnpfwbyqoeatv"
  , "rmyxgdlihczskvnpfabkqoeatv"
  , "rmyxgdlihczskunpfwhjwzeatv"
  , "jlyxgdlihczskunpfwbjqzeatv"
  , "rmyxgdlihccskunpfwwjqopatv"
  , "rmyxgxlihczskuupfwbjqoeahv"
  , "rmyxgdcihcbskungfwbjqoeatv"
  , "tmyxgdlihczskunpfwbjmoeftv"
  , "rkyxgdlioczskmnpfwbjqoeatv"
  , "rmyxgdlrhczskulpfwbjaoeatv"
  , "rmysgdlihczikunphwbjqoeatv"
  , "rmyxgdlihczskuvpfwbjqoeyty"
  , "fmyxgdlihczscunpfqbjqoeatv"
  , "rfyxgdlihzzrkunpfwbjqoeatv"
  , "rmyxgdlikczskunpfwbjqolath"
  , "rmyxqdlihjzskunpfwbjqoeamv"
  , "rmuxodiihczskunpfwbjqoeatv"
  , "rmyygdliucuskunpfwbjqoeatv"
  , "rmyxgdliwczskuppawbjqoeatv"
  , "rmyxgdlihczskunprwbjqgehtv"
  , "imyvgdlihczskunpfwbjqouatv"
  , "rgyxgdluhczskunpflbjqoeatv"
  , "rmgxgdlihczsdunpfwwjqoeatv"
  , "gdyxgdlihczskunpfwbjqoeavv"
  , "rmyxgdlihczskunpfwljjoektv"
  , "rmexgdlihczskunpfwxjqoeytv"
  , "rmyxqdlihcyskuwpfwbjqoeatv"
  , "rmyxgdlihczskunpfiyjqcebtv"
  , "amyngdlihczskunpfwbjqseatv"
  , "rmzxgdlihczykubpfwbjqoeatv"
  , "rmyxgdlihczhkuopfwbjsoeatv"
  , "rmyxgdlihczskunpfwbaqowztv"
  , "rmgxgdlihczslunpfwbjeoeatv"
  , "rmytgdlzhczskunrfwbjqoeatv"
  , "rmyxgdtihczskunafobjqoeatv"
  , "rmyxgdlihczskuflfbbjqoeatv"
  , "rmdxgdlihczskunpfwbjqoealj"
  , "rbyxgdlihczskuppdwbjqoeatv"
  , "rmyxhdiihcwskunpfwbjqoeatv"
  , "rmmggdlfhczskunpfwbjqoeatv"
  , "rmbxgblihczskuypfwbjqoeatv"
  , "rmyxgslihczsjunpjwbjqoeatv"
  , "rmyxgdlohczsaunpfwbjboeatv"
  , "rmaxgdhihczskunpfwbjooeatv"
  , "rmyxidlihczskunpfgbuqoeatv"
  , "rmyxgdlihfzckznpfwbjqoeatv"
  , "rmaqgdpihczskunpfwbjqoeatv"
  , "rmyvgdlirczskunpfobjqoeatv"
  , "rmdxgdlihczlkunpxwbjqoeatv"
  , "rmyxgdlihczseunpfwbjvdeatv"
  , "rmyxgdlihczskuhpfwbjqneath"
  , "rmyxrdlihciskunpfwbjqoratv"
  , "rmyxgdmihczsqunpftbjqoeatv"
  , "rmyxgdlbhczskulpfbbjqoeatv"
  , "rmoxgdlihczskunpfwbjqoeesv"
  , "rmyxgdlihczskuijfwejqoeatv"
  , "rmyxgdlihczskunpfwnkqoxatv"
  , "rmyxgdvihmzskuupfwbjqoeatv"
  , "rkyxedlihczskunpfcbjqoeatv"
  , "rmyxgdjihczskunprwbjqieatv"
  , "omyxgqgihczskunpfwbjqoeatv"
  , "rmyxydlihczskunpfwkjqoentv"
  , "rmbxgdlicczskunpfwbjqteatv"
  , "emyxgdlihczskugpfwbjqneatv"
  , "dmyxgflihczskunpfwbjqjeatv"
  , "umyxgdlihczskunpfwbjloextv"
  , "rmyxgdlihczsbunpfwbyqpeatv"
  , "rmyxgdrihczsvunpcwbjqoeatv"
  , "qmyxgdlihcwsknnpfwbjqoeatv"
  , "ymyxgdlihczskunpfsbjqowatv"
  , "rmyxgdlbhczskunpnvbjqoeatv"
  , "rmyxfdlixczskunpfwbjqoertv"
  , "rmyygdlihszrkunpfwbjqoeatv"
  , "rmyxgxlihcpskunpfwbjqoeanv"
  , "rmyxgdlihczskjnpfwbjqoprtv"
  , "rmyxgdlisczfkunpfwbjqoeath"
  , "rmyxgdlihczskunpfkbjqoeaji"
  , "rmyxgylihczskunpfwbfqoeatl"
  , "rmsxgdbihczskunpfwtjqoeatv"
  , "smyxgdlihczskunpfwbjqcwatv"
  , "rmyxgdlihczskunppjljqoeatv"
  , "rmyxgdlihczskulpfdbjooeatv"
  , "rmyxgdlihczskunpfibjqcebtv"
  , "rmyxadlihczskunpgwbjyoeatv"
  , "rmyxgdlihczdkunpvwbjqoeytv"
  , "rmyxgdlihcvskunpfwbjxohatv"
  , "rmyxgplihczskunpfgbjqoeauv"
  , "rmyxgdlihcysrunmfwbjqoeatv"
  , "rmyygdlihczskunpfwbjqvewtv"
  , "rmyxgdlihczsmunpfwdjnoeatv"
  , "rmyxgdbibczskunpfwbjuoeatv"
  , "rmyfgdlihczskubpfwbjqoeatp"
  , "rmyxgdlihczskuopfzijqoeatv"
  , "rmyqgdlihczskunpwwbjqoeanv"
  , "imyxgdlihczskunpfwbjqoqytv"
  , "rmyxgdlixcoskbnpfwbjqoeatv"
  , "rmyxgrlihccskunpfwbjqteatv"
  , "rdyxgdlihcpskunpfwbjqoratv"
  , "rmyxgdlihkzskunpfwbjmoeatj"
  , "rmyxgslihczskcnpfjbjqoeatv"
  , "rmyxgdlihczsqunqfwdjqoeatv"
  , "rjyxgdlyhczbkunpfwbjqoeatv"
  , "rmyxudlihczjkunpfwbjqzeatv"
  ]
