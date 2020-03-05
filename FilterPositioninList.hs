d :: [Int] -> [Int]
d (x:xs) = x:(f xs)
d [] = []

f :: [Int] -> [Int]
f (x:xs) = d xs
f [] = []

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
	inputdata <- getContents
	mapM_ (putStrLn. show). f. map read. lines $ inputdata
