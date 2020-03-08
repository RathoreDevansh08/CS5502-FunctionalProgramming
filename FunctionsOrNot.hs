import qualified Data.List as L  

repeatIO :: Int ->IO a -> IO [a]
repeatIO n action = sequence $ take n $ repeat action

main = do
        tS <- getLine
        let tI  = read tS :: Int
        repeatIO tI testcase  

testcase :: IO ()
testcase = do 
            nS <-  getLine
            let nI = read nS :: Int
            ls <- repeatIO nI getLine
            let tuples = fmap readTuple ls :: [(Int,Int)]
            if isFunc tuples then putStrLn "YES" else putStrLn "NO"    

readTuple :: Read a => String -> (a,a)
readTuple str = (,) (read $ takeWhile (/=' ')  str) (read $ dropWhile (/=' ') str) 

uniqueSort :: Ord a => [a]->[a]
uniqueSort xs = L.sort $ L.nub xs 

isFunc :: Ord a => [(a,a)] -> Bool
isFunc ts = let tsc = cycle $ uniqueSort ts
                fstEq (a,_) (b,_) = (a==b)
            in  not $ or $ take (length ts) $ zipWith (fstEq) tsc $ tail tsc
