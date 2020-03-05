f :: Int -> [Int] -> [Int]
f n (a:ar) = if a<n
                then a:(f n ar)
                else (f n ar)
f n [] = []

-- The Input/Output section. You do not need to change or modify this part
main = do 
    n <- readLn :: IO Int 
    inputdata <- getContents 
    let 
        numbers = map read (lines inputdata) :: [Int] 
    putStrLn . unlines $ (map show . f n) numbers
