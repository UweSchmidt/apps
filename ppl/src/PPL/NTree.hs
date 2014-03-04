module PPL.NTree where

data NTree a = NTree a [NTree a]

type StringNTree = NTree String

formatNTree :: (a -> String) -> String -> String -> NTree a -> String

formatNTree node2String pf1 pf2 (NTree n l)
       = formatNode
         ++ formatChildren pf2 l
         where
         formatNode     = pf1 ++ concat (map trNL (node2String n)) ++ "\n"
         trNL '\n'      = "\n" ++ pf2
         trNL c         = [c]
         formatChildren _ [] = ""
         formatChildren pf (t:ts)
             | null ts   = pfl
                           ++ formatTr pf2' t
             | otherwise = pfl
                           ++ formatTr pf1' t
                           ++ formatChildren pf ts
             where
             pf0        = pf ++ indent1
             pf1'       = pf ++ indent2
             pf2'       = pf ++ indent3
             pfl        = pf ++ indent4
             formatTr   = formatNTree node2String pf0
         indent1 = "+---"
         indent2 = "|   "
         indent3 = "    "
         indent4 = "|\n"

formatStdNTree          :: (a -> String) -> NTree a -> String
formatStdNTree node2String
    = formatNTree node2String "---" "   "

formatStringNTree       :: NTree String -> String
formatStringNTree
    = formatNTree id "---" "   "

