f (a:ar) = if a `mod` 2 == 0
                then f ar
                else a + (f ar)
f [] = 0

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
	inputdata <- getContents
	putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata
