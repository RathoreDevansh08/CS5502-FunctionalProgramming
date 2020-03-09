data Tree a = Nulltree
            | Node (Tree a) a (Tree a)
                        deriving (Show)

mapAccumL :: (acc -> a -> (b, acc)) -> acc -> [a] -> ([b], acc)
mapAccumL f acc [] = ([], acc)
mapAccumL f acc1 (x:xs) = (y, acc3)
       where
            (b0, acc2) = f acc1 x
            (bs, acc3) = mapAccumL f acc2 xs
            y = b0 : bs
                       

mapAccumR :: (acc -> a -> (b, acc)) -> acc -> [a] -> ([b], acc)
mapAccumR f acc [] = ([], acc)
mapAccumR f acc1 (x:xs) = (y, acc3)
       where
            (bs, acc2) = mapAccumR f acc1 xs
            (b0, acc3) = f acc2 x
            y = b0 : bs


main = do
    putStrLn "Its me"
