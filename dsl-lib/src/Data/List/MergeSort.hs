module Data.List.MergeSort
    ( mergeSortByM
    , mergeSortUniqueByM
    , natMergeSortBy
    , natMergeSortUniqueBy
    , sortByM'
    )
where

-- ------------------------------------------------------------

sortByM' :: (Monad m) => Bool -> Bool -> (a -> a -> m Ordering) -> [a] -> m [a]
sortByM' natural unique cmp l
    | null l          = return l
    | null . tail $ l = return l
    | otherwise       = ( if natural
                          then splitNatural
                          else splitBinary) l >>= mergeLists >>= return . head
    where
      splitBinary
          = return . map (:[])

      splitNatural (x1 : xs@(x2 : _))
          = do o <- cmp x1 x2
               case o of
                 LT -> do (ys : yss) <- splitNatural xs
                          return ((x1 : ys) : yss)
                 GT -> do yss <- splitNatural xs
                          return ([x1] : yss)
                 EQ -> do (ys : yss) <- splitNatural xs
                          return (mergeop x1 ys : yss)
      splitNatural xs
          = return [xs]

      mergeop
          | unique    = const id
          | otherwise = (:)

      mergeLists (xs1 : xs2 : xss)
          = do ys1 <- merge xs1 xs2
               yss <- mergeLists xss
               mergeLists (ys1 : yss)
      mergeLists res
          = return res

      merge xs@(x : xs') ys@(y : ys')
          = do o <- cmp x y
               case o of
                 LT -> do zs <- merge xs' ys
                          return (x : zs)
                 GT -> do zs <- merge xs  ys'
                          return (y : zs)
                 EQ -> do zs <- merge xs' ys
                          return $ mergeop x zs
      merge [] ys
          = return ys
      merge xs []
          = return xs

mergeSortByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m [a]
mergeSortByM = sortByM' False False

mergeSortUniqueByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m [a]
mergeSortUniqueByM = sortByM' False True

natMergeSortBy :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m [a]
natMergeSortBy = sortByM' True False

natMergeSortUniqueBy :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m [a]
natMergeSortUniqueBy = sortByM' True True

-- ------------------------------------------------------------
