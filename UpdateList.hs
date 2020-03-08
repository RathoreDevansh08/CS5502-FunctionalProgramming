-- Enter your code here. Read input from STDIN. Print output to STDOUT

f (a:ar) = if (a<0) 
                then (-1 * a) : f ar
                else a : f ar
f [] = []

-- This section handles the Input/Output and can be used as it is. Do not modify it.
main = do
	inputdata <- getContents
	mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
